/// Point-to-Point Channel - OTP actor-based message queue
/// Each message is delivered to exactly one consumer
import auditor/event.{type AuditEvent}
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/otp/actor

/// Messages that can be sent to the channel
pub type ChannelMessage {
  /// Send an event to the channel
  Send(event: AuditEvent)
  /// Request to receive an event (with reply subject)
  Receive(reply_to: Subject(Result(AuditEvent, Nil)))
  /// Get the current queue length
  QueueLength(reply_to: Subject(Int))
}

/// Channel state - just a queue of events
pub type ChannelState {
  ChannelState(queue: List(AuditEvent))
}

/// Start the channel actor
pub fn start() -> Result(actor.Started(Subject(ChannelMessage)), actor.StartError) {
  actor.new(ChannelState(queue: []))
  |> actor.on_message(handle_message)
  |> actor.start
}

/// Send an event to the channel
pub fn send(channel: Subject(ChannelMessage), event: AuditEvent) -> Nil {
  actor.send(channel, Send(event))
}

/// Receive an event from the channel (blocking with timeout)
pub fn receive(
  channel: Subject(ChannelMessage),
  timeout_ms: Int,
) -> Result(AuditEvent, Nil) {
  actor.call(channel, timeout_ms, Receive)
}

/// Get the queue length
pub fn queue_length(channel: Subject(ChannelMessage), timeout_ms: Int) -> Int {
  actor.call(channel, timeout_ms, QueueLength)
}

/// Handle incoming messages
fn handle_message(
  state: ChannelState,
  message: ChannelMessage,
) -> actor.Next(ChannelState, ChannelMessage) {
  case message {
    Send(event) -> {
      // Add to the end of the queue
      let new_queue = list.append(state.queue, [event])
      actor.continue(ChannelState(queue: new_queue))
    }

    Receive(reply_to) -> {
      case state.queue {
        [] -> {
          // No events available
          process.send(reply_to, Error(Nil))
          actor.continue(state)
        }
        [first, ..rest] -> {
          // Return the first event, remove from queue
          process.send(reply_to, Ok(first))
          actor.continue(ChannelState(queue: rest))
        }
      }
    }

    QueueLength(reply_to) -> {
      process.send(reply_to, list.length(state.queue))
      actor.continue(state)
    }
  }
}
