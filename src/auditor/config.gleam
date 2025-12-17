//// Configuration Module - 12-Factor App Configuration
////
//// Follows the 12-factor app methodology for configuration:
//// - All configuration from environment variables
//// - No config files in code
//// - Clear defaults for development
////
//// Environment Variables:
//// - CHRONICLE_TRANSPORT: "otp" (default) or "rabbitmq"
//// - CHRONICLE_PORT: HTTP server port (default: 8080)
//// - CHRONICLE_CONSUMER_COUNT: Number of competing consumers (default: 3)
//// - RABBITMQ_HOST: RabbitMQ host (default: localhost)
//// - RABBITMQ_PORT: RabbitMQ port (default: 5672)
//// - RABBITMQ_USER: RabbitMQ username (default: guest)
//// - RABBITMQ_PASS: RabbitMQ password (default: guest)
//// - RABBITMQ_VHOST: RabbitMQ vhost (default: /)
//// - RABBITMQ_QUEUE: Queue name (default: chronicle.events)

import envoy
import gleam/int
import gleam/result

/// Transport backend for the message channel
pub type Transport {
  /// OTP-based in-process channel (default, no external dependencies)
  Otp
  /// RabbitMQ-based channel (requires RabbitMQ server)
  RabbitMQ
}

/// RabbitMQ connection configuration
pub type RabbitConfig {
  RabbitConfig(
    host: String,
    port: Int,
    user: String,
    password: String,
    vhost: String,
    queue: String,
  )
}

/// Main application configuration
pub type Config {
  Config(
    transport: Transport,
    port: Int,
    consumer_count: Int,
    rabbitmq: RabbitConfig,
  )
}

/// Load configuration from environment variables
/// Falls back to sensible defaults for local development
pub fn load() -> Config {
  Config(
    transport: load_transport(),
    port: load_port(),
    consumer_count: load_consumer_count(),
    rabbitmq: load_rabbitmq_config(),
  )
}

/// Load transport type from CHRONICLE_TRANSPORT
fn load_transport() -> Transport {
  case envoy.get("CHRONICLE_TRANSPORT") {
    Ok("rabbitmq") -> RabbitMQ
    Ok("rabbit") -> RabbitMQ
    Ok("rmq") -> RabbitMQ
    _ -> Otp
  }
}

/// Load HTTP server port from CHRONICLE_PORT
fn load_port() -> Int {
  envoy.get("CHRONICLE_PORT")
  |> result.try(int.parse)
  |> result.unwrap(8080)
}

/// Load consumer count from CHRONICLE_CONSUMER_COUNT
fn load_consumer_count() -> Int {
  envoy.get("CHRONICLE_CONSUMER_COUNT")
  |> result.try(int.parse)
  |> result.unwrap(3)
}

/// Load RabbitMQ configuration from environment
fn load_rabbitmq_config() -> RabbitConfig {
  RabbitConfig(
    host: envoy.get("RABBITMQ_HOST") |> result.unwrap("localhost"),
    port: envoy.get("RABBITMQ_PORT")
      |> result.try(int.parse)
      |> result.unwrap(5672),
    user: envoy.get("RABBITMQ_USER") |> result.unwrap("guest"),
    password: envoy.get("RABBITMQ_PASS") |> result.unwrap("guest"),
    vhost: envoy.get("RABBITMQ_VHOST") |> result.unwrap("/"),
    queue: envoy.get("RABBITMQ_QUEUE") |> result.unwrap("chronicle.events"),
  )
}

/// Get a human-readable description of the transport
pub fn transport_name(transport: Transport) -> String {
  case transport {
    Otp -> "OTP (in-process)"
    RabbitMQ -> "RabbitMQ"
  }
}

/// Check if using RabbitMQ transport
pub fn is_rabbitmq(config: Config) -> Bool {
  case config.transport {
    RabbitMQ -> True
    Otp -> False
  }
}

/// Format RabbitMQ connection string (for logging, masks password)
pub fn rabbitmq_connection_string(rabbit: RabbitConfig) -> String {
  "amqp://"
  <> rabbit.user
  <> ":***@"
  <> rabbit.host
  <> ":"
  <> int.to_string(rabbit.port)
  <> rabbit.vhost
}

