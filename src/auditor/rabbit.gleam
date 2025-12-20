//// RabbitMQ Backend - carotte-based message transport
////
//// Implements the messaging backend using RabbitMQ via carotte.
//// This module provides connection management, publishing, and consuming
//// capabilities that integrate with our Messaging Gateway.
////
//// Supports both simple queue-based messaging and topic exchange routing
//// for Datatype Channel pattern implementation.

import auditor/config.{type RabbitConfig}
import auditor/event.{type AuditEvent}
import auditor/log
import auditor/routing_config.{type RoutingConfig}
import auditor/topology
import carotte
import carotte/channel.{type Channel, Channel}
import carotte/publisher
import carotte/queue
import gleam/dynamic/decode
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/json
import gleam/option.{None, Some}
import gleam/result

/// RabbitMQ connection state
pub type RabbitConnection {
  RabbitConnection(
    client: carotte.Client,
    channel: Channel,
    queue_name: String,
    // For topic exchange routing (Datatype Channel)
    exchange: option.Option(String),
  )
}

/// Connect to RabbitMQ and set up the queue (simple mode)
pub fn connect(config: RabbitConfig) -> Result(RabbitConnection, String) {
  log.info(
    "Connecting to RabbitMQ at "
    <> config.host
    <> ":"
    <> int.to_string(config.port),
  )

  // Build the client connection
  let client_result =
    carotte.default_client()
    |> carotte.with_host(config.host)
    |> carotte.with_port(config.port)
    |> carotte.with_username(config.user)
    |> carotte.with_password(config.password)
    |> carotte.with_virtual_host(config.vhost)
    |> carotte.start()

  case client_result {
    Ok(client) -> {
      log.info("Connected to RabbitMQ")

      // Open a channel
      case channel.open_channel(client) {
        Ok(ch) -> {
          log.info("Channel opened")

          // Declare the durable queue
          let q =
            queue.new(config.queue)
            |> queue.as_durable()

          case queue.declare(q, ch) {
            Ok(declared) -> {
              log.info("Queue declared: " <> declared.name)
              Ok(RabbitConnection(
                client: client,
                channel: ch,
                queue_name: declared.name,
                exchange: None,
              ))
            }
            Error(e) -> {
              let _ = carotte.close(client)
              Error("Failed to declare queue: " <> carotte_error_to_string(e))
            }
          }
        }
        Error(e) -> {
          let _ = carotte.close(client)
          Error("Failed to open channel: " <> carotte_error_to_string(e))
        }
      }
    }
    Error(e) ->
      Error("Failed to connect to RabbitMQ: " <> carotte_error_to_string(e))
  }
}

/// Connect to RabbitMQ with full topology setup (Datatype Channel mode)
/// Sets up exchanges, queues, and bindings from routing configuration
pub fn connect_with_topology(
  config: RabbitConfig,
  routing: RoutingConfig,
) -> Result(RabbitConnection, String) {
  log.info(
    "Connecting to RabbitMQ with topology at "
    <> config.host
    <> ":"
    <> int.to_string(config.port),
  )

  let client_result =
    carotte.default_client()
    |> carotte.with_host(config.host)
    |> carotte.with_port(config.port)
    |> carotte.with_username(config.user)
    |> carotte.with_password(config.password)
    |> carotte.with_virtual_host(config.vhost)
    |> carotte.start()

  case client_result {
    Ok(client) -> {
      log.info("Connected to RabbitMQ")

      case channel.open_channel(client) {
        Ok(ch) -> {
          log.info("Channel opened, setting up topology...")

          // Set up the full topology from config
          case topology.ensure_topology(ch, routing) {
            Ok(_) -> {
              // Use the first exchange as default for publishing
              let exchange_name = case routing.routes {
                [first, ..] -> Some(first.exchange)
                [] -> None
              }
              let queue_name = case routing.routes {
                [first, ..] -> first.queue
                [] -> config.queue
              }

              Ok(RabbitConnection(
                client: client,
                channel: ch,
                queue_name: queue_name,
                exchange: exchange_name,
              ))
            }
            Error(e) -> {
              let _ = carotte.close(client)
              Error("Failed to set up topology: " <> e)
            }
          }
        }
        Error(e) -> {
          let _ = carotte.close(client)
          Error("Failed to open channel: " <> carotte_error_to_string(e))
        }
      }
    }
    Error(e) ->
      Error("Failed to connect to RabbitMQ: " <> carotte_error_to_string(e))
  }
}

/// Publish an audit event to RabbitMQ
/// Uses topic exchange with routing key if configured (Datatype Channel)
/// Falls back to direct queue publish otherwise
pub fn publish(conn: RabbitConnection, evt: AuditEvent) -> Result(Nil, String) {
  let payload = event_to_json(evt)

  // Determine exchange and routing key based on connection mode
  let #(exchange, routing_key) = case conn.exchange {
    Some(ex) -> #(ex, event.routing_key(evt))
    None -> #("", conn.queue_name)
  }

  log.debug("Publishing to " <> exchange <> " with key: " <> routing_key)

  publisher.publish(
    channel: conn.channel,
    exchange: exchange,
    routing_key: routing_key,
    payload: payload,
    options: [
      publisher.Persistent(True),
      publisher.ContentType("application/json"),
      publisher.MessageId(evt.id),
      // Include event_type as message type for consumers
      publisher.Type(event.routing_key(evt)),
    ],
  )
  |> result.map_error(fn(e) {
    "Failed to publish: " <> carotte_error_to_string(e)
  })
}

/// Subscribe to a specific queue (for consumer roles)
pub fn subscribe_to_queue(
  conn: RabbitConnection,
  queue_name: String,
  callback: fn(AuditEvent) -> Nil,
) -> Result(String, String) {
  // Set QoS prefetch=1 for fair dispatch
  let Channel(pid: channel_pid) = conn.channel
  let _ = set_prefetch(channel_pid, 1)

  queue.subscribe_with_options(
    channel: conn.channel,
    queue: queue_name,
    options: [queue.AutoAck(True)],
    callback: fn(payload, deliver) {
      case json_to_event(payload.payload) {
        Ok(evt) -> callback(evt)
        Error(_) ->
          log.error("Failed to parse event from queue: " <> payload.payload)
      }
      let _ = queue.ack_single(conn.channel, deliver.delivery_tag)
      Nil
    },
  )
  |> result.map_error(fn(e) {
    "Failed to subscribe to " <> queue_name <> ": " <> carotte_error_to_string(e)
  })
}

/// Subscribe to the queue and process events with a callback
/// Uses prefetch=1 and manual ack for fair dispatch among competing consumers
pub fn subscribe(
  conn: RabbitConnection,
  callback: fn(AuditEvent) -> Nil,
) -> Result(String, String) {
  // Set QoS prefetch=1 for fair dispatch
  let Channel(pid: channel_pid) = conn.channel
  let _ = set_prefetch(channel_pid, 1)

  // Manual ack mode (AutoAck(True) = "I will ack manually")
  queue.subscribe_with_options(
    channel: conn.channel,
    queue: conn.queue_name,
    options: [queue.AutoAck(True)],
    callback: fn(payload, deliver) {
      case json_to_event(payload.payload) {
        Ok(event) -> callback(event)
        Error(_) ->
          log.error("Failed to parse event from queue: " <> payload.payload)
      }
      let _ = queue.ack_single(conn.channel, deliver.delivery_tag)
      Nil
    },
  )
  |> result.map_error(fn(e) {
    "Failed to subscribe: " <> carotte_error_to_string(e)
  })
}

/// Set QoS prefetch count on a channel (for fair dispatch)
@external(erlang, "rabbit_qos_ffi", "set_prefetch")
fn set_prefetch(channel_pid: Pid, prefetch_count: Int) -> Result(Nil, String)

/// Close the RabbitMQ connection
pub fn close(conn: RabbitConnection) -> Result(Nil, String) {
  carotte.close(conn.client)
  |> result.map_error(fn(e) {
    "Failed to close connection: " <> carotte_error_to_string(e)
  })
}

/// Serialize an audit event to JSON string
fn event_to_json(event: AuditEvent) -> String {
  json.to_string(event.to_json(event))
}

/// Deserialize JSON string to audit event
fn json_to_event(payload: String) -> Result(AuditEvent, Nil) {
  let decoder = {
    use id <- decode.field("id", decode.string)
    use actor <- decode.field("actor", decode.string)
    use action <- decode.field("action", decode.string)
    use resource_type <- decode.field("resource_type", decode.string)
    use resource_id <- decode.field("resource_id", decode.string)
    use timestamp <- decode.field("timestamp", decode.string)
    use event_type <- decode.optional_field(
      "event_type",
      None,
      decode.optional(decode.string),
    )

    // If event_type is present, use new_with_type, otherwise derive it
    case event_type {
      Some(et) ->
        decode.success(event.new_with_type(
          id,
          actor,
          action,
          resource_type,
          resource_id,
          timestamp,
          et,
        ))
      None ->
    decode.success(event.new(
      id,
      actor,
      action,
      resource_type,
      resource_id,
      timestamp,
    ))
    }
  }
  json.parse(payload, decoder)
  |> result.replace_error(Nil)
}

/// Convert carotte error to string for logging
fn carotte_error_to_string(error: carotte.CarotteError) -> String {
  case error {
    carotte.Blocked -> "Connection blocked"
    carotte.Closed -> "Connection closed"
    carotte.AuthFailure(msg) -> "Auth failure: " <> msg
    carotte.ProcessNotFound -> "Process not found"
    carotte.AlreadyRegistered(name) -> "Already registered: " <> name
    carotte.NotFound(msg) -> "Not found: " <> msg
    carotte.AccessRefused(msg) -> "Access refused: " <> msg
    carotte.PreconditionFailed(msg) -> "Precondition failed: " <> msg
    carotte.ResourceLocked(msg) -> "Resource locked: " <> msg
    carotte.ChannelClosed(msg) -> "Channel closed: " <> msg
    carotte.ConnectionRefused(msg) -> "Connection refused: " <> msg
    carotte.ConnectionTimeout(msg) -> "Connection timeout: " <> msg
    carotte.FrameError(msg) -> "Frame error: " <> msg
    carotte.InternalError(msg) -> "Internal error: " <> msg
    carotte.InvalidPath(msg) -> "Invalid path: " <> msg
    carotte.NoRoute(msg) -> "No route: " <> msg
    carotte.NotAllowed(msg) -> "Not allowed: " <> msg
    carotte.NotImplemented(msg) -> "Not implemented: " <> msg
    carotte.UnexpectedFrame(msg) -> "Unexpected frame: " <> msg
    carotte.CommandInvalid(msg) -> "Command invalid: " <> msg
    carotte.UnknownError(msg) -> "Unknown error: " <> msg
  }
}
