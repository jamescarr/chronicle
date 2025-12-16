//// Consumer - reads from channel and writes to store
////
//// Demonstrates the Point-to-Point pattern: each message is delivered
//// to exactly one consumer, ensuring no duplicate processing.
////
//// With multiple consumers (Competing Consumers pattern), messages are
//// distributed across consumers - each message still goes to exactly one.

import auditor/channel.{type ChannelMessage}
import auditor/log
import auditor/store.{type Table}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/otp/actor

/// Messages the consumer can receive
pub type ConsumerMessage {
  Poll
  Stop
}

/// Consumer state
pub type ConsumerState {
  ConsumerState(name: String, channel: Subject(ChannelMessage), store: Table)
}

/// Start a named consumer actor with autonomous polling
pub fn start_named(
  name: String,
  ch: Subject(ChannelMessage),
  st: Table,
) -> Result(actor.Started(Subject(ConsumerMessage)), actor.StartError) {
  log.info("Starting consumer: " <> name)
  actor.new(ConsumerState(name: name, channel: ch, store: st))
  |> actor.on_message(handle_message)
  |> actor.start
}

/// Start a consumer actor (default name)
pub fn start(
  ch: Subject(ChannelMessage),
  st: Table,
) -> Result(actor.Started(Subject(ConsumerMessage)), actor.StartError) {
  start_named("consumer-1", ch, st)
}

/// Start multiple competing consumers
pub fn start_pool(
  count: Int,
  ch: Subject(ChannelMessage),
  st: Table,
) -> List(Subject(ConsumerMessage)) {
  list.range(1, count)
  |> list.filter_map(fn(i) {
    case start_named("consumer-" <> int.to_string(i), ch, st) {
      Ok(started) -> Ok(started.data)
      Error(_) -> Error(Nil)
    }
  })
}

/// Trigger the consumer to poll for messages
pub fn poll(consumer: Subject(ConsumerMessage)) -> Nil {
  actor.send(consumer, Poll)
}

/// Poll all consumers in a pool (triggers competition!)
pub fn poll_all(consumers: List(Subject(ConsumerMessage))) -> Nil {
  // Shuffle to randomize which consumer gets polled first
  // This creates true competition for messages
  consumers
  |> list.shuffle
  |> list.each(fn(c) { actor.send(c, Poll) })
}

fn handle_message(
  state: ConsumerState,
  message: ConsumerMessage,
) -> actor.Next(ConsumerState, ConsumerMessage) {
  case message {
    Poll -> {
      // Try to receive - only ONE consumer will get the message
      case channel.receive(state.channel, 50) {
        Ok(event) -> {
          log.info(
            "["
            <> state.name
            <> "] Processing "
            <> event.id
            <> " - "
            <> event.action,
          )
          let _ = store.insert(state.store, event)
          actor.continue(state)
        }
        Error(Nil) -> {
          // No message available (another consumer got it, or queue empty)
          actor.continue(state)
        }
      }
    }
    Stop -> actor.stop()
  }
}
