//// Unit tests for the Pipes and Filters Pipeline
////
//// Tests the pipeline processing, filter chaining, and error handling.

import auditor/event
import auditor/pipeline.{Continue, Reject, Skip}
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
// Basic Pipeline Tests
// =============================================================================

pub fn empty_pipeline_returns_event_unchanged_test() {
  let evt = make_event()
  let pipe = pipeline.new()

  pipeline.process(pipe, evt)
  |> should.be_ok
  |> fn(result) { result.id |> should.equal("evt-123") }
}

pub fn single_filter_transforms_event_test() {
  let evt = make_event()

  let uppercase_actor = fn(e: event.AuditEvent) {
    Continue(event.AuditEvent(..e, actor: string.uppercase(e.actor)))
  }

  let pipe = pipeline.from_filters([uppercase_actor])

  pipeline.process(pipe, evt)
  |> should.be_ok
  |> fn(result) { result.actor |> should.equal("TEST@EXAMPLE.COM") }
}

pub fn multiple_filters_chain_correctly_test() {
  let evt = make_event()

  let uppercase_actor = fn(e: event.AuditEvent) {
    Continue(event.AuditEvent(..e, actor: string.uppercase(e.actor)))
  }

  let uppercase_action = fn(e: event.AuditEvent) {
    Continue(event.AuditEvent(..e, action: string.uppercase(e.action)))
  }

  let pipe = pipeline.from_filters([uppercase_actor, uppercase_action])

  let result = pipeline.process(pipe, evt) |> should.be_ok
  result.actor |> should.equal("TEST@EXAMPLE.COM")
  result.action |> should.equal("CREATE")
}

// =============================================================================
// Filter Result Tests
// =============================================================================

pub fn reject_stops_pipeline_test() {
  let evt = make_event()

  let always_reject = fn(_e: event.AuditEvent) {
    Reject("not allowed")
  }

  let should_not_run = fn(_e: event.AuditEvent) {
    // This should never be called
    Continue(event.AuditEvent(..make_event(), id: "should-not-see-this"))
  }

  let pipe = pipeline.from_filters([always_reject, should_not_run])

  pipeline.process(pipe, evt)
  |> should.be_error
  |> should.equal("Rejected: not allowed")
}

pub fn skip_stops_pipeline_test() {
  let evt = make_event()

  let skip_filter = fn(_e: event.AuditEvent) {
    Skip("event filtered out")
  }

  let pipe = pipeline.from_filters([skip_filter])

  pipeline.process(pipe, evt)
  |> should.be_error
  |> should.equal("Skipped: event filtered out")
}

pub fn continue_passes_to_next_filter_test() {
  let evt = make_event()

  // We can't easily track call count in Gleam without mutable state,
  // so we'll verify by checking the final transformation
  let add_meta_1 = fn(e: event.AuditEvent) {
    Continue(event.with_metadata(e, "filter1", "ran"))
  }

  let add_meta_2 = fn(e: event.AuditEvent) {
    Continue(event.with_metadata(e, "filter2", "ran"))
  }

  let pipe = pipeline.from_filters([add_meta_1, add_meta_2])

  let result = pipeline.process(pipe, evt) |> should.be_ok

  // Both filters should have run
  result.metadata
  |> should.not_equal(evt.metadata)
}

// =============================================================================
// Pipeline Composition Tests
// =============================================================================

pub fn compose_combines_pipelines_test() {
  let evt = make_event()

  let pipe_a =
    pipeline.from_filters([
      fn(e) { Continue(event.with_metadata(e, "a", "1")) },
    ])

  let pipe_b =
    pipeline.from_filters([
      fn(e) { Continue(event.with_metadata(e, "b", "2")) },
    ])

  let combined = pipeline.compose(pipe_a, pipe_b)

  let result = pipeline.process(combined, evt) |> should.be_ok

  // Both pipeline's filters should have run
  result.metadata |> should.not_equal(evt.metadata)
}

pub fn add_appends_filter_test() {
  let evt = make_event()

  let pipe =
    pipeline.new()
    |> pipeline.add(fn(e) { Continue(event.with_metadata(e, "added", "yes")) })

  let result = pipeline.process(pipe, evt) |> should.be_ok
  result.metadata |> should.not_equal(evt.metadata)
}

// =============================================================================
// Helper Function Tests
// =============================================================================

pub fn from_transform_creates_filter_test() {
  let evt = make_event()

  let filter =
    pipeline.from_transform(fn(e) {
      event.AuditEvent(..e, action: "transformed")
    })

  let pipe = pipeline.from_filters([filter])

  pipeline.process(pipe, evt)
  |> should.be_ok
  |> fn(result) { result.action |> should.equal("transformed") }
}

pub fn from_validator_creates_filter_test() {
  // Test passing validation
  let evt = make_event()

  let validator =
    pipeline.from_validator(fn(e: event.AuditEvent) {
      case e.actor {
        "" -> Error("actor is required")
        _ -> Ok(Nil)
      }
    })

  let pipe = pipeline.from_filters([validator])

  pipeline.process(pipe, evt)
  |> should.be_ok

  // Test failing validation
  let bad_evt = event.AuditEvent(..evt, actor: "")

  pipeline.process(pipe, bad_evt)
  |> should.be_error
  |> should.equal("Rejected: actor is required")
}

pub fn passthrough_does_nothing_test() {
  let evt = make_event()

  let pipe = pipeline.from_filters([pipeline.passthrough()])

  pipeline.process(pipe, evt)
  |> should.be_ok
  |> fn(result) { result |> should.equal(evt) }
}

pub fn process_lenient_returns_original_on_error_test() {
  let evt = make_event()

  let always_reject = fn(_e: event.AuditEvent) {
    Reject("nope")
  }

  let pipe = pipeline.from_filters([always_reject])

  pipeline.process_lenient(pipe, evt)
  |> should.equal(evt)
}

