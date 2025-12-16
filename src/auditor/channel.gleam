//// Point-to-Point Channel - OTP actor-based message queue
////
//// Implements the Point-to-Point Channel pattern from Enterprise Integration
//// Patterns. Each message is delivered to exactly one consumer, ensuring
//// no duplicate processing.

import auditor/event.{type AuditEvent}
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/otp/actor

/// Messages the channel accepts
pub type ChannelMessage {
  Send(event: AuditEvent)
  Receive(reply_to: Subject(Result(AuditEvent, Nil)))
  QueueLength(reply_to: Subject(Int))
}

/// Channel state - a FIFO queue of events
pub type ChannelState {
  ChannelState(queue: List(AuditEvent))
}

/// Start the channel actor
pub fn start() -> Result(
  actor.Started(Subject(ChannelMessage)),
  actor.StartError,
) {
  actor.new(ChannelState(queue: []))
  |> actor.on_message(handle_message)
  |> actor.start
}

/// Send an event to the channel (async, fire-and-forget)
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

/// Get the current queue length
pub fn queue_length(channel: Subject(ChannelMessage), timeout_ms: Int) -> Int {
  actor.call(channel, timeout_ms, QueueLength)
}

fn handle_message(
  state: ChannelState,
  message: ChannelMessage,
) -> actor.Next(ChannelState, ChannelMessage) {
  case message {
    Send(event) -> {
      let new_queue = list.append(state.queue, [event])
      actor.continue(ChannelState(queue: new_queue))
    }

    Receive(reply_to) -> {
      case state.queue {
        [] -> {
          process.send(reply_to, Error(Nil))
          actor.continue(state)
        }
        [first, ..rest] -> {
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
