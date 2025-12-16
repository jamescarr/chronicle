//// Consumer - reads from channel and writes to store
////
//// Demonstrates the Point-to-Point pattern: each message is delivered
//// to exactly one consumer, ensuring no duplicate processing.

import auditor/channel.{type ChannelMessage}
import auditor/log
import auditor/store.{type Table}
import gleam/erlang/process.{type Subject}
import gleam/otp/actor

/// Messages the consumer can receive
pub type ConsumerMessage {
  Poll
  Stop
}

/// Consumer state
pub type ConsumerState {
  ConsumerState(channel: Subject(ChannelMessage), store: Table)
}

/// Start the consumer actor
pub fn start(
  ch: Subject(ChannelMessage),
  st: Table,
) -> Result(actor.Started(Subject(ConsumerMessage)), actor.StartError) {
  actor.new(ConsumerState(channel: ch, store: st))
  |> actor.on_message(handle_message)
  |> actor.start
}

/// Trigger the consumer to poll for messages
pub fn poll(consumer: Subject(ConsumerMessage)) -> Nil {
  actor.send(consumer, Poll)
}

fn handle_message(
  state: ConsumerState,
  message: ConsumerMessage,
) -> actor.Next(ConsumerState, ConsumerMessage) {
  case message {
    Poll -> {
      case channel.receive(state.channel, 100) {
        Ok(event) -> {
          log.info("Processing event " <> event.id <> " - " <> event.action)
          let _ = store.insert(state.store, event)
          actor.continue(state)
        }
        Error(Nil) -> actor.continue(state)
      }
    }
    Stop -> actor.stop()
  }
}
