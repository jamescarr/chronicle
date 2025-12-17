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
//// The application code (router, consumers) interacts only with the gateway,
//// remaining blissfully unaware of the underlying transport mechanism.

import auditor/channel
import auditor/config.{type Config, type RabbitConfig, Otp, RabbitMQ}
import auditor/event.{type AuditEvent}
import auditor/log
import auditor/rabbit
import gleam/erlang/process.{type Subject}
import gleam/result

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

/// Start a messaging gateway based on configuration
pub fn start(config: Config) -> Result(GatewayResult, String) {
  case config.transport {
    Otp -> start_otp_gateway()
    RabbitMQ -> start_rabbit_gateway(config.rabbitmq)
  }
}

/// Start an OTP-based gateway (uses our existing channel)
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

/// Start a RabbitMQ gateway
fn start_rabbit_gateway(
  rabbit_config: RabbitConfig,
) -> Result(GatewayResult, String) {
  log.info(
    "Starting RabbitMQ gateway: "
    <> config.rabbitmq_connection_string(rabbit_config),
  )

  rabbit.connect(rabbit_config)
  |> result.map(fn(connection) {
    GatewayResult(
      gateway: RabbitGateway(connection: connection),
      transport_name: "RabbitMQ (" <> rabbit_config.queue <> ")",
    )
  })
}

/// Send an event through the gateway (fire-and-forget)
/// This is the domain-specific method the Gateway pattern calls for
pub fn send_event(gateway: Gateway, event: AuditEvent) -> Nil {
  case gateway {
    OtpGateway(channel:) -> {
      channel.send(channel, event)
    }
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

/// Receive an event from the gateway (blocking with timeout)
/// Used by consumers to pull messages (only for OTP mode)
/// RabbitMQ uses push-based subscription via subscribe_events
pub fn receive_event(
  gateway: Gateway,
  timeout_ms: Int,
) -> Result(AuditEvent, Nil) {
  case gateway {
    OtpGateway(channel:) -> {
      channel.receive(channel, timeout_ms)
    }
    RabbitGateway(_) -> {
      // RabbitMQ uses push-based consumers, not pull
      // Use subscribe_events() instead
      Error(Nil)
    }
  }
}

/// Subscribe to events (RabbitMQ mode)
/// The callback is invoked for each event received
pub fn subscribe_events(
  gateway: Gateway,
  callback: fn(AuditEvent) -> Nil,
) -> Result(String, String) {
  case gateway {
    OtpGateway(_) -> {
      // OTP mode doesn't need subscription - use receive_event + polling
      Error("OTP gateway uses pull-based receive_event, not subscription")
    }
    RabbitGateway(connection:) -> {
      rabbit.subscribe(connection, callback)
    }
  }
}

/// Get the current queue length (only meaningful for OTP backend)
pub fn queue_length(gateway: Gateway, timeout_ms: Int) -> Int {
  case gateway {
    OtpGateway(channel:) -> channel.queue_length(channel, timeout_ms)
    RabbitGateway(_) -> {
      // RabbitMQ queue length would require management API
      0
    }
  }
}

/// Check if the gateway is using RabbitMQ (distributed mode)
pub fn is_distributed(gateway: Gateway) -> Bool {
  case gateway {
    OtpGateway(_) -> False
    RabbitGateway(_) -> True
  }
}

/// Get the underlying OTP channel subject (for backwards compatibility)
/// This will be removed once we fully migrate to gateway-based consumers
pub fn get_otp_channel(
  gateway: Gateway,
) -> Result(Subject(channel.ChannelMessage), Nil) {
  case gateway {
    OtpGateway(channel:) -> Ok(channel)
    RabbitGateway(_) -> Error(Nil)
  }
}

/// Get the RabbitMQ connection (for advanced use cases)
pub fn get_rabbit_connection(
  gateway: Gateway,
) -> Result(rabbit.RabbitConnection, Nil) {
  case gateway {
    OtpGateway(_) -> Error(Nil)
    RabbitGateway(connection:) -> Ok(connection)
  }
}
