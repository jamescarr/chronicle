//// Messaging Gateway - Encapsulates messaging system access
////
//// From Enterprise Integration Patterns:
//// > A Messaging Gateway is a class that wraps messaging-specific method calls
//// > and exposes domain-specific methods to the application.
////
//// This gateway provides a unified interface for sending and receiving
//// audit events, abstracting whether we're using:
//// - OTP actors (in-process, default)
//// - RabbitMQ (distributed messaging)
////
//// The application code interacts only with the gateway,
//// remaining blissfully unaware of the underlying transport mechanism.
//// Even consumer lifecycle is managed through the gateway!

import auditor/channel
import auditor/config.{type Config, type RabbitConfig, Otp, RabbitMQ}
import auditor/consumer
import auditor/event.{type AuditEvent}
import auditor/event_store.{type EventStore}
import auditor/log
import auditor/rabbit
import auditor/routing_config
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/option
import gleam/otp/actor
import gleam/result

// =============================================================================
// Gateway Types
// =============================================================================

/// The Gateway handle - opaque to callers
/// This is what makes it a true Gateway pattern - callers don't know the internals
pub opaque type Gateway {
  OtpGateway(channel: Subject(channel.ChannelMessage))
  RabbitGateway(connection: rabbit.RabbitConnection)
}

/// Result of starting the gateway
pub type GatewayResult {
  GatewayResult(gateway: Gateway, transport_name: String)
}

/// Consumer pool - opaque handle to running consumers
/// Client code doesn't need to know if these are OTP actors or RabbitMQ subscriptions
pub opaque type ConsumerPool {
  OtpConsumerPool(consumers: List(Subject(ConsumerMessage)))
  RabbitConsumerPool(consumer_tag: String)
}

// =============================================================================
// Gateway Lifecycle
// =============================================================================

/// Start a messaging gateway based on configuration
pub fn start(config: Config) -> Result(GatewayResult, String) {
  case config.transport {
    Otp -> start_otp_gateway()
    RabbitMQ -> start_rabbit_gateway(config.rabbitmq)
  }
}

fn start_otp_gateway() -> Result(GatewayResult, String) {
  case channel.start() {
    Ok(started) -> {
      log.info("Started OTP messaging gateway")
      Ok(GatewayResult(
        gateway: OtpGateway(channel: started.data),
        transport_name: "OTP (in-process)",
      ))
    }
    Error(_) -> Error("Failed to start OTP channel actor")
  }
}

fn start_rabbit_gateway(
  rabbit_config: RabbitConfig,
) -> Result(GatewayResult, String) {
  log.info(
    "Starting RabbitMQ gateway: "
    <> config.rabbitmq_connection_string(rabbit_config),
  )

  // Load routing configuration for topology setup
  let routing = routing_config.load_or_default()
  log.info(
    "Loaded routing config with "
    <> int.to_string(list.length(routing.routes))
    <> " routes",
  )

  rabbit.connect_with_topology(rabbit_config, routing)
  |> result.map(fn(connection) {
    GatewayResult(
      gateway: RabbitGateway(connection: connection),
      transport_name: "RabbitMQ (exchange: audit_events)",
    )
  })
}

// =============================================================================
// Sending Events (Producer side)
// =============================================================================

/// Send an event through the gateway (fire-and-forget)
/// Works identically for OTP and RabbitMQ - true transport abstraction!
pub fn send_event(gateway: Gateway, event: AuditEvent) -> Nil {
  case gateway {
    OtpGateway(channel:) -> channel.send(channel, event)
    RabbitGateway(connection:) -> {
      case rabbit.publish(connection, event) {
        Ok(_) -> Nil
        Error(msg) -> {
          log.error("Failed to publish to RabbitMQ: " <> msg)
          Nil
        }
      }
    }
  }
}

// =============================================================================
// Consumer Management (Consumer side)
// =============================================================================

/// Start consumers for this gateway with a specific role
/// The role determines which queue(s) to subscribe to:
/// - "default" or "analytics" → chronicle.all (routing key #, receives all events)
/// - "security" → chronicle.security (routing key security.#)
/// - "billing" → chronicle.billing (routing key billing.#)
/// - "users" → chronicle.users (routing key user.#)
pub fn start_consumers(
  gateway: Gateway,
  count: Int,
  store: EventStore,
) -> Result(ConsumerPool, String) {
  start_consumers_with_role(gateway, count, store, "default")
}

/// Start consumers with a specific role for queue binding
pub fn start_consumers_with_role(
  gateway: Gateway,
  count: Int,
  store: EventStore,
  role: String,
) -> Result(ConsumerPool, String) {
  case gateway {
    OtpGateway(channel:) -> start_otp_consumers(channel, count, store)
    RabbitGateway(connection:) ->
      start_rabbit_consumers(connection, store, role)
  }
}

/// Start OTP-based consumers that poll the channel
fn start_otp_consumers(
  ch: Subject(channel.ChannelMessage),
  count: Int,
  store: EventStore,
) -> Result(ConsumerPool, String) {
  let consumers =
    list.range(1, count)
    |> list.filter_map(fn(i) {
      let name = "consumer-" <> int.to_string(i)
      case start_consumer_actor(name, ch, store) {
        Ok(started) -> Ok(started.data)
        Error(_) -> Error(Nil)
      }
    })

  log.info(
    "Started " <> int.to_string(list.length(consumers)) <> " OTP consumers",
  )
  Ok(OtpConsumerPool(consumers: consumers))
}

/// Start RabbitMQ consumer - subscribes to queue based on role
fn start_rabbit_consumers(
  connection: rabbit.RabbitConnection,
  store: EventStore,
  role: String,
) -> Result(ConsumerPool, String) {
  let consumer_name = node_name()
  let handler =
    consumer.create_handler(consumer.ConsumerConfig(
      name: consumer_name,
      store: store,
    ))

  // Look up the queue for this consumer role
  let routing = routing_config.load_or_default()
  let queue_name = get_queue_for_role(routing, role)

  log.info(
    "Consumer role '" <> role <> "' subscribing to queue: " <> queue_name,
  )

  rabbit.subscribe_to_queue(connection, queue_name, handler)
  |> result.map(fn(tag) { RabbitConsumerPool(consumer_tag: tag) })
}

/// Get the queue name for a consumer role
fn get_queue_for_role(
  routing: routing_config.RoutingConfig,
  role: String,
) -> String {
  // Look up the consumer config for this role
  case routing_config.get_consumer(routing, role) {
    option.Some(consumer_config) -> {
      // Get the first route for this consumer
      case consumer_config.routes {
        [first_route_name, ..] -> {
          // Look up the route to get the queue name
          case routing_config.get_route(routing, first_route_name) {
            option.Some(route) -> route.queue
            option.None -> "chronicle.all"
            // Fallback to catch-all
          }
        }
        [] -> "chronicle.all"
        // No routes configured, use catch-all
      }
    }
    option.None -> "chronicle.all"
    // Unknown role, use catch-all
  }
}

@external(erlang, "auditor_gateway_ffi", "get_node_name")
fn node_name() -> String

/// Notify consumers that new messages may be available
/// For OTP: triggers polling (competing consumers race to grab messages)
/// For RabbitMQ: no-op (RabbitMQ pushes messages automatically)
pub fn notify_consumers(pool: ConsumerPool) -> Nil {
  case pool {
    OtpConsumerPool(consumers:) -> {
      // Shuffle to randomize which consumer polls first - true competition!
      consumers
      |> list.shuffle
      |> list.each(fn(c) { actor.send(c, Poll) })
    }
    RabbitConsumerPool(_) -> {
      // RabbitMQ pushes messages to consumers automatically
      Nil
    }
  }
}

/// Get the number of active consumers
pub fn consumer_count(pool: ConsumerPool) -> Int {
  case pool {
    OtpConsumerPool(consumers:) -> list.length(consumers)
    RabbitConsumerPool(_) -> 1
    // RabbitMQ has one subscription
  }
}

// =============================================================================
// Internal Consumer Actor (OTP mode only)
// =============================================================================

/// Messages the consumer actor can receive
type ConsumerMessage {
  Poll
  Stop
}

/// Consumer actor state
type ConsumerState {
  ConsumerState(
    name: String,
    channel: Subject(channel.ChannelMessage),
    store: EventStore,
  )
}

fn start_consumer_actor(
  name: String,
  ch: Subject(channel.ChannelMessage),
  store: EventStore,
) -> Result(actor.Started(Subject(ConsumerMessage)), actor.StartError) {
  log.info("Starting consumer: " <> name)
  actor.new(ConsumerState(name: name, channel: ch, store: store))
  |> actor.on_message(handle_consumer_message)
  |> actor.start
}

fn handle_consumer_message(
  state: ConsumerState,
  message: ConsumerMessage,
) -> actor.Next(ConsumerState, ConsumerMessage) {
  case message {
    Poll -> {
      case channel.receive(state.channel, 50) {
        Ok(event) -> {
          consumer.ingest(event, state.store, state.name)
          actor.continue(state)
        }
        Error(Nil) -> actor.continue(state)
      }
    }
    Stop -> actor.stop()
  }
}

// =============================================================================
// Utility Functions
// =============================================================================

/// Check if the gateway is using distributed messaging (RabbitMQ)
pub fn is_distributed(gateway: Gateway) -> Bool {
  case gateway {
    OtpGateway(_) -> False
    RabbitGateway(_) -> True
  }
}

/// Get the current queue length (only meaningful for OTP backend)
pub fn queue_length(gateway: Gateway, timeout_ms: Int) -> Int {
  case gateway {
    OtpGateway(channel:) -> channel.queue_length(channel, timeout_ms)
    RabbitGateway(_) -> 0
  }
}
