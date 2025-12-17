//// RabbitMQ Backend - carotte-based message transport
////
//// Implements the messaging backend using RabbitMQ via carotte.
//// This module provides connection management, publishing, and consuming
//// capabilities that integrate with our Messaging Gateway.

import auditor/config.{type RabbitConfig}
import auditor/event.{type AuditEvent}
import auditor/log
import carotte
import carotte/channel.{type Channel}
import carotte/publisher
import carotte/queue
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/result

/// RabbitMQ connection state
pub type RabbitConnection {
  RabbitConnection(client: carotte.Client, channel: Channel, queue_name: String)
}

/// Connect to RabbitMQ and set up the queue
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

/// Publish an audit event to RabbitMQ
pub fn publish(conn: RabbitConnection, event: AuditEvent) -> Result(Nil, String) {
  let payload = event_to_json(event)

  publisher.publish(
    channel: conn.channel,
    exchange: "",
    // Direct to queue (default exchange)
    routing_key: conn.queue_name,
    payload: payload,
    options: [
      publisher.Persistent(True),
      publisher.ContentType("application/json"),
      publisher.MessageId(event.id),
    ],
  )
  |> result.map_error(fn(e) {
    "Failed to publish: " <> carotte_error_to_string(e)
  })
}

/// Subscribe to the queue and process events with a callback
pub fn subscribe(
  conn: RabbitConnection,
  callback: fn(AuditEvent) -> Nil,
) -> Result(String, String) {
  queue.subscribe(
    channel: conn.channel,
    queue: conn.queue_name,
    callback: fn(payload, _deliver) {
      case json_to_event(payload.payload) {
        Ok(event) -> callback(event)
        Error(_) -> {
          log.error("Failed to parse event from queue: " <> payload.payload)
          Nil
        }
      }
    },
  )
  |> result.map_error(fn(e) {
    "Failed to subscribe: " <> carotte_error_to_string(e)
  })
}

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
    decode.success(event.AuditEvent(
      id: id,
      actor: actor,
      action: action,
      resource_type: resource_type,
      resource_id: resource_id,
      timestamp: timestamp,
    ))
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
