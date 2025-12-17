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
import gleam/erlang/process.{type Subject}

/// The Gateway handle - opaque to callers
/// This is what makes it a true Gateway pattern - callers don't know the internals
pub opaque type Gateway {
  OtpGateway(channel: Subject(channel.ChannelMessage))
  RabbitGateway(config: RabbitConfig)
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
/// NOTE: Actual RabbitMQ connection will be implemented when we add carotte
fn start_rabbit_gateway(rabbit_config: RabbitConfig) -> Result(GatewayResult, String) {
  log.info(
    "Starting RabbitMQ gateway: "
    <> config.rabbitmq_connection_string(rabbit_config),
  )
  // Placeholder - will be implemented with carotte
  // For now, return the gateway handle that will be used for rabbit operations
  Ok(GatewayResult(
    gateway: RabbitGateway(config: rabbit_config),
    transport_name: "RabbitMQ (" <> rabbit_config.queue <> ")",
  ))
}

/// Send an event through the gateway (fire-and-forget)
/// This is the domain-specific method the Gateway pattern calls for
pub fn send_event(gateway: Gateway, event: AuditEvent) -> Nil {
  case gateway {
    OtpGateway(channel:) -> {
      channel.send(channel, event)
    }
    RabbitGateway(config:) -> {
      // Placeholder for RabbitMQ publish
      log.debug(
        "Would publish to RabbitMQ queue: "
        <> config.queue
        <> " event: "
        <> event.id,
      )
      Nil
    }
  }
}

/// Receive an event from the gateway (blocking with timeout)
/// Used by consumers to pull messages
pub fn receive_event(
  gateway: Gateway,
  timeout_ms: Int,
) -> Result(AuditEvent, Nil) {
  case gateway {
    OtpGateway(channel:) -> {
      channel.receive(channel, timeout_ms)
    }
    RabbitGateway(_config) -> {
      // Placeholder for RabbitMQ consume
      // RabbitMQ will use push-based consumers via carotte
      Error(Nil)
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

/// Check if the gateway is using RabbitMQ
pub fn is_distributed(gateway: Gateway) -> Bool {
  case gateway {
    OtpGateway(_) -> False
    RabbitGateway(_) -> True
  }
}

/// Get the underlying OTP channel subject (for backwards compatibility)
/// This will be removed once we fully migrate to gateway-based consumers
pub fn get_otp_channel(gateway: Gateway) -> Result(Subject(channel.ChannelMessage), Nil) {
  case gateway {
    OtpGateway(channel:) -> Ok(channel)
    RabbitGateway(_) -> Error(Nil)
  }
}

