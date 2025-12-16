/// Consumer - reads from channel and writes to store
/// This demonstrates the Point-to-Point pattern: one consumer per message
import auditor/channel.{type ChannelMessage}
import auditor/store.{type Table}
import gleam/erlang/process.{type Subject}
import gleam/io
import gleam/otp/actor

/// Consumer messages
pub type ConsumerMessage {
  /// Process the next message from the channel
  Poll
  /// Stop the consumer
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

/// Handle consumer messages
fn handle_message(
  state: ConsumerState,
  message: ConsumerMessage,
) -> actor.Next(ConsumerState, ConsumerMessage) {
  case message {
    Poll -> {
      // Try to receive from the channel
      case channel.receive(state.channel, 100) {
        Ok(event) -> {
          // Got an event, store it
          io.println(
            "Consumer: Processing event " <> event.id <> " - " <> event.action,
          )
          let _ = store.insert(state.store, event)
          // Continue accepting messages
          actor.continue(state)
        }
        Error(Nil) -> {
          // No event available
          actor.continue(state)
        }
      }
    }

    Stop -> {
      actor.stop()
    }
  }
}
