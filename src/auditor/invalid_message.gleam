//// Invalid Message Channel
////
//// Handles malformed, expired, or undeliverable messages by routing them
//// to a dead letter queue (DLQ) for later inspection and potential replay.
////
//// From Enterprise Integration Patterns:
//// > What will the messaging system do with a message it cannot deliver?
//// > Move it to an Invalid Message Channel so it can be inspected.
////
//// This implementation uses RabbitMQ's dead letter exchange (DLX) feature:
//// - Messages that are rejected (nack with requeue=false) go to the DLQ
//// - Messages that expire (TTL) go to the DLQ
//// - Messages from queues that overflow go to the DLQ
////
//// The DLQ can be monitored, and messages can be:
//// 1. Inspected for debugging
//// 2. Fixed and republished
//// 3. Discarded after review

import auditor/log
import auditor/routing_config.{type RoutingConfig}
import carotte
import carotte/channel.{type Channel}
import carotte/publisher
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

// =============================================================================
// Types
// =============================================================================

/// Reason why a message was sent to the dead letter queue
pub type InvalidReason {
  /// Message could not be parsed as JSON
  ParseError(error_message: String)
  /// Message parsed but was missing required fields
  ValidationError(missing_fields: List(String))
  /// Processing failed after the message was parsed
  ProcessingError(error_message: String)
  /// Message was rejected by a consumer
  Rejected
  /// Message expired (TTL)
  Expired
  /// Unknown reason (shouldn't happen)
  Unknown
}

/// A dead-lettered message with context
pub type DeadLetter {
  DeadLetter(
    original_payload: String,
    reason: InvalidReason,
    original_exchange: String,
    original_routing_key: String,
    original_queue: String,
    death_count: Int,
    first_death_time: Option(String),
  )
}

// =============================================================================
// Sending to DLQ
// =============================================================================

/// Manually send a message to the dead letter queue
/// Use this when a message can't be processed but shouldn't be lost
pub fn send_to_dlq(
  ch: Channel,
  dlq_name: String,
  payload: String,
  reason: InvalidReason,
) -> Result(Nil, String) {
  log.info("Sending invalid message to DLQ: " <> dlq_name)
  log.debug("Reason: " <> reason_to_string(reason))

  // Publish directly to the DLQ
  publisher.publish(
    channel: ch,
    exchange: "",
    routing_key: dlq_name,
    payload: payload,
    options: [
      publisher.Persistent(True),
      publisher.ContentType("application/json"),
      // Add headers with rejection reason
      publisher.Type(reason_to_string(reason)),
    ],
  )
  |> result.map_error(fn(e) { carotte_error_to_string(e) })
}

/// Reject a message and let RabbitMQ route it to the DLQ
/// This uses the queue's configured dead-letter-exchange
@external(erlang, "auditor_invalid_message_ffi", "reject")
pub fn reject(ch: Channel, delivery_tag: Int, requeue: Bool) -> Result(Nil, String)

/// Reject a message (don't requeue) - will go to DLQ if configured
pub fn nack_to_dlq(ch: Channel, delivery_tag: Int) -> Result(Nil, String) {
  reject(ch, delivery_tag, False)
}

// =============================================================================
// Monitoring DLQ
// =============================================================================

/// Get all messages from a DLQ for inspection
/// Note: This consumes the messages - use with caution in production
pub fn inspect_dlq(
  ch: Channel,
  dlq_name: String,
  limit: Int,
) -> List(DeadLetter) {
  log.info("Inspecting DLQ: " <> dlq_name <> " (limit: " <> int.to_string(limit) <> ")")
  
  // Use basic.get to retrieve messages without consuming
  do_get_messages(ch, dlq_name, limit, [])
}

fn do_get_messages(
  ch: Channel,
  queue_name: String,
  remaining: Int,
  acc: List(DeadLetter),
) -> List(DeadLetter) {
  case remaining <= 0 {
    True -> list.reverse(acc)
    False -> {
      case get_single_message(ch, queue_name) {
        Ok(Some(dl)) -> do_get_messages(ch, queue_name, remaining - 1, [dl, ..acc])
        Ok(None) -> list.reverse(acc)
        Error(_) -> list.reverse(acc)
      }
    }
  }
}

@external(erlang, "auditor_invalid_message_ffi", "get_message")
fn get_single_message(ch: Channel, queue_name: String) -> Result(Option(DeadLetter), String)

// =============================================================================
// Replaying Messages
// =============================================================================

/// Republish a dead-lettered message to its original exchange
/// Use this after fixing the underlying issue that caused the rejection
pub fn replay_message(
  ch: Channel,
  dead_letter: DeadLetter,
) -> Result(Nil, String) {
  log.info(
    "Replaying message to " <> dead_letter.original_exchange <> " with key: " <> dead_letter.original_routing_key,
  )

  publisher.publish(
    channel: ch,
    exchange: dead_letter.original_exchange,
    routing_key: dead_letter.original_routing_key,
    payload: dead_letter.original_payload,
    options: [
      publisher.Persistent(True),
      publisher.ContentType("application/json"),
    ],
  )
  |> result.map_error(fn(e) { carotte_error_to_string(e) })
}

/// Replay all messages from a DLQ back to their original exchanges
/// Returns the count of replayed messages
pub fn replay_all(ch: Channel, dlq_name: String) -> Result(Int, String) {
  log.info("Replaying all messages from DLQ: " <> dlq_name)
  
  let messages = inspect_dlq(ch, dlq_name, 1000)
  let count = list.length(messages)
  
  use _ <- result.try(
    messages
    |> list.try_each(fn(msg) { replay_message(ch, msg) })
  )
  
  log.info("Replayed " <> int.to_string(count) <> " messages")
  Ok(count)
}

// =============================================================================
// DLQ Stats
// =============================================================================

/// Get stats for all configured DLQs
pub fn get_dlq_stats(ch: Channel, config: RoutingConfig) -> List(#(String, Int)) {
  config.routes
  |> list.filter_map(fn(route) {
    case route.dead_letter_queue {
      Some(dlq_name) -> {
        case get_queue_depth(ch, dlq_name) {
          Ok(depth) -> Ok(#(dlq_name, depth))
          Error(_) -> Error(Nil)
        }
      }
      None -> Error(Nil)
    }
  })
}

@external(erlang, "auditor_invalid_message_ffi", "get_queue_depth")
fn get_queue_depth(ch: Channel, queue_name: String) -> Result(Int, String)

// =============================================================================
// Helpers
// =============================================================================

pub fn reason_to_string(reason: InvalidReason) -> String {
  case reason {
    ParseError(msg) -> "parse_error: " <> msg
    ValidationError(fields) ->
      "validation_error: missing " <> string_join(fields, ", ")
    ProcessingError(msg) -> "processing_error: " <> msg
    Rejected -> "rejected"
    Expired -> "expired"
    Unknown -> "unknown"
  }
}

fn string_join(list: List(String), sep: String) -> String {
  case list {
    [] -> ""
    [x] -> x
    [x, ..rest] -> x <> sep <> string_join(rest, sep)
  }
}

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

