//// Unit tests for the Point-to-Point Channel
////
//// These tests define the expected behavior of our message channel:
//// - Messages are delivered in FIFO order
//// - Each message is delivered to exactly one consumer (Point-to-Point)
//// - The channel supports async send and sync receive
//// - Queue length can be queried

import auditor/channel
import auditor/event.{type AuditEvent, AuditEvent}
import gleam/erlang/process
import gleam/int
import gleam/list
import gleeunit/should

// =============================================================================
// Helper Functions
// =============================================================================

fn make_event(id: String, action: String) -> AuditEvent {
  AuditEvent(
    id: id,
    actor: "test@example.com",
    action: action,
    resource_type: "test",
    resource_id: "test-123",
    timestamp: "2025-12-16T00:00:00Z",
  )
}

// =============================================================================
// Channel Behavior Tests
// =============================================================================

pub fn channel_starts_with_empty_queue_test() {
  let assert Ok(started) = channel.start()
  let ch = started.data

  channel.queue_length(ch, 1000)
  |> should.equal(0)
}

pub fn channel_send_increases_queue_length_test() {
  let assert Ok(started) = channel.start()
  let ch = started.data

  // Send one event
  channel.send(ch, make_event("evt-1", "create"))
  process.sleep(10)

  channel.queue_length(ch, 1000)
  |> should.equal(1)

  // Send another
  channel.send(ch, make_event("evt-2", "update"))
  process.sleep(10)

  channel.queue_length(ch, 1000)
  |> should.equal(2)
}

pub fn channel_receive_returns_first_message_fifo_test() {
  let assert Ok(started) = channel.start()
  let ch = started.data

  // Send events in order
  channel.send(ch, make_event("first", "create"))
  channel.send(ch, make_event("second", "update"))
  channel.send(ch, make_event("third", "delete"))
  process.sleep(10)

  // Receive should return them in FIFO order
  let assert Ok(event1) = channel.receive(ch, 1000)
  event1.id |> should.equal("first")

  let assert Ok(event2) = channel.receive(ch, 1000)
  event2.id |> should.equal("second")

  let assert Ok(event3) = channel.receive(ch, 1000)
  event3.id |> should.equal("third")
}

pub fn channel_receive_removes_message_from_queue_test() {
  let assert Ok(started) = channel.start()
  let ch = started.data

  channel.send(ch, make_event("evt-1", "create"))
  channel.send(ch, make_event("evt-2", "update"))
  process.sleep(10)

  channel.queue_length(ch, 1000)
  |> should.equal(2)

  // Receive one
  let assert Ok(_) = channel.receive(ch, 1000)

  // Queue should now have 1
  channel.queue_length(ch, 1000)
  |> should.equal(1)
}

pub fn channel_receive_from_empty_returns_error_test() {
  let assert Ok(started) = channel.start()
  let ch = started.data

  // Empty queue should return Error
  channel.receive(ch, 100)
  |> should.equal(Error(Nil))
}

pub fn channel_point_to_point_delivery_test() {
  // This test verifies the Point-to-Point guarantee:
  // Each message is delivered to exactly ONE receiver

  let assert Ok(started) = channel.start()
  let ch = started.data

  // Send a single message
  channel.send(ch, make_event("single", "create"))
  process.sleep(10)

  // First receive gets the message
  let result1 = channel.receive(ch, 1000)
  result1 |> should.be_ok

  // Second receive on same channel gets nothing
  let result2 = channel.receive(ch, 100)
  result2 |> should.equal(Error(Nil))
}

pub fn channel_preserves_event_data_test() {
  let assert Ok(started) = channel.start()
  let ch = started.data

  let original =
    AuditEvent(
      id: "test-id-123",
      actor: "alice@company.com",
      action: "archive",
      resource_type: "document",
      resource_id: "doc-456",
      timestamp: "2025-12-16T12:00:00Z",
    )

  channel.send(ch, original)
  process.sleep(10)

  let assert Ok(received) = channel.receive(ch, 1000)

  received.id |> should.equal("test-id-123")
  received.actor |> should.equal("alice@company.com")
  received.action |> should.equal("archive")
  received.resource_type |> should.equal("document")
  received.resource_id |> should.equal("doc-456")
  received.timestamp |> should.equal("2025-12-16T12:00:00Z")
}

pub fn channel_handles_many_messages_test() {
  let assert Ok(started) = channel.start()
  let ch = started.data

  // Send 100 messages
  list.range(1, 100)
  |> list.each(fn(i) {
    let id = "evt-" <> int.to_string(i)
    channel.send(ch, make_event(id, "bulk"))
  })

  process.sleep(50)

  channel.queue_length(ch, 1000)
  |> should.equal(100)

  // Receive all 100
  list.range(1, 100)
  |> list.each(fn(_) {
    let result = channel.receive(ch, 1000)
    result |> should.be_ok
  })

  // Queue should be empty
  channel.queue_length(ch, 1000)
  |> should.equal(0)
}
