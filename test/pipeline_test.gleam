//// Unit tests for the Pipes and Filters Pipeline
////
//// Tests filter chaining using Gleam's pipe operator and result.try

import auditor/event
import auditor/pipeline
import gleam/result
import gleam/string
import gleeunit/should

// =============================================================================
// Helper Functions
// =============================================================================

fn make_event() -> event.AuditEvent {
  event.new(
    "evt-123",
    "test@example.com",
    "create",
    "document",
    "doc-456",
    "2025-12-17T00:00:00Z",
  )
}

// =============================================================================
// Direct Pipe Tests (the idiomatic way!)
// =============================================================================

pub fn direct_pipe_transforms_event_test() {
  let uppercase_actor = fn(e: event.AuditEvent) {
    Ok(event.AuditEvent(..e, actor: string.uppercase(e.actor)))
  }

  make_event()
  |> uppercase_actor
  |> should.be_ok
  |> fn(result) { result.actor |> should.equal("TEST@EXAMPLE.COM") }
}

pub fn chained_pipes_transform_sequentially_test() {
  let uppercase_actor = fn(e: event.AuditEvent) {
    Ok(event.AuditEvent(..e, actor: string.uppercase(e.actor)))
  }

  let uppercase_action = fn(e: event.AuditEvent) {
    Ok(event.AuditEvent(..e, action: string.uppercase(e.action)))
  }

  let result =
    Ok(make_event())
    |> result.try(uppercase_actor)
    |> result.try(uppercase_action)
    |> should.be_ok

  result.actor |> should.equal("TEST@EXAMPLE.COM")
  result.action |> should.equal("CREATE")
}

pub fn error_stops_pipeline_test() {
  let always_fail = fn(_e: event.AuditEvent) { Error("not allowed") }

  let should_not_run = fn(_e: event.AuditEvent) {
    Ok(event.AuditEvent(..make_event(), id: "should-not-see-this"))
  }

  Ok(make_event())
  |> result.try(always_fail)
  |> result.try(should_not_run)
  |> should.be_error
  |> should.equal("not allowed")
}

pub fn ok_continues_pipeline_test() {
  let add_meta_1 = fn(e: event.AuditEvent) {
    Ok(event.with_metadata(e, "filter1", "ran"))
  }

  let add_meta_2 = fn(e: event.AuditEvent) {
    Ok(event.with_metadata(e, "filter2", "ran"))
  }

  let evt = make_event()

  let result =
    Ok(evt)
    |> result.try(add_meta_1)
    |> result.try(add_meta_2)
    |> should.be_ok

  // Both filters should have run
  result.metadata |> should.not_equal(evt.metadata)
}

// =============================================================================
// List Runner Tests (for dynamic composition)
// =============================================================================

pub fn run_with_empty_list_returns_event_test() {
  let evt = make_event()

  pipeline.run(evt, [])
  |> should.be_ok
  |> fn(result) { result.id |> should.equal("evt-123") }
}

pub fn run_with_single_filter_test() {
  let evt = make_event()

  let uppercase_actor = fn(e: event.AuditEvent) {
    Ok(event.AuditEvent(..e, actor: string.uppercase(e.actor)))
  }

  pipeline.run(evt, [uppercase_actor])
  |> should.be_ok
  |> fn(result) { result.actor |> should.equal("TEST@EXAMPLE.COM") }
}

pub fn run_chains_multiple_filters_test() {
  let evt = make_event()

  let uppercase_actor = fn(e: event.AuditEvent) {
    Ok(event.AuditEvent(..e, actor: string.uppercase(e.actor)))
  }

  let uppercase_action = fn(e: event.AuditEvent) {
    Ok(event.AuditEvent(..e, action: string.uppercase(e.action)))
  }

  let result =
    pipeline.run(evt, [uppercase_actor, uppercase_action])
    |> should.be_ok

  result.actor |> should.equal("TEST@EXAMPLE.COM")
  result.action |> should.equal("CREATE")
}

pub fn run_stops_on_error_test() {
  let evt = make_event()

  let always_fail = fn(_e: event.AuditEvent) { Error("nope") }

  let should_not_run = fn(_e: event.AuditEvent) {
    Ok(event.AuditEvent(..make_event(), id: "should-not-see-this"))
  }

  pipeline.run(evt, [always_fail, should_not_run])
  |> should.be_error
  |> should.equal("nope")
}

pub fn run_lenient_returns_original_on_error_test() {
  let evt = make_event()

  let always_fail = fn(_e: event.AuditEvent) { Error("nope") }

  pipeline.run_lenient(evt, [always_fail])
  |> should.equal(evt)
}
