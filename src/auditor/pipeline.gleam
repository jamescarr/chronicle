//// Pipeline - Pipes and Filters pattern implementation
////
//// From Enterprise Integration Patterns:
//// > "Divide a larger processing task into a sequence of smaller,
//// > independent processing steps (Filters) that are connected by
//// > channels (Pipes)."
////
//// A Pipeline chains Filters together. Each Filter transforms an event,
//// potentially enriching, validating, or modifying it. The output of
//// one filter becomes the input to the next.
////
//// Example:
////   event
////   |> validate_required()    -- ensure required fields present
////   |> normalize_actor()      -- lowercase email
////   |> add_correlation_id()   -- add tracing ID if missing
////   |> enrich_from_entity()   -- lookup entity metadata
////   |> log_event()            -- log for debugging

import auditor/event.{type AuditEvent}
import gleam/list
import gleam/result

/// Result of applying a filter
pub type FilterResult {
  /// Event was transformed successfully
  Continue(event: AuditEvent)
  /// Event failed validation - stop processing
  Reject(reason: String)
  /// Event should be skipped (not an error, just filtered out)
  Skip(reason: String)
}

/// A Filter transforms an event and returns a FilterResult
pub type Filter =
  fn(AuditEvent) -> FilterResult

/// A Pipeline is an ordered list of filters
pub type Pipeline =
  List(Filter)

/// Create an empty pipeline
pub fn new() -> Pipeline {
  []
}

/// Add a filter to the pipeline
pub fn add(pipeline: Pipeline, filter: Filter) -> Pipeline {
  list.append(pipeline, [filter])
}

/// Chain multiple filters into a pipeline
pub fn from_filters(filters: List(Filter)) -> Pipeline {
  filters
}

/// Process an event through all filters in the pipeline
/// Returns Ok(event) if successful, Error with reason if rejected/skipped
pub fn process(
  pipeline: Pipeline,
  event: AuditEvent,
) -> Result(AuditEvent, String) {
  do_process(pipeline, event)
}

fn do_process(
  remaining: List(Filter),
  event: AuditEvent,
) -> Result(AuditEvent, String) {
  case remaining {
    [] -> Ok(event)
    [filter, ..rest] -> {
      case filter(event) {
        Continue(transformed) -> do_process(rest, transformed)
        Reject(reason) -> Error("Rejected: " <> reason)
        Skip(reason) -> Error("Skipped: " <> reason)
      }
    }
  }
}

/// Process an event, returning the original on error (lenient mode)
pub fn process_lenient(pipeline: Pipeline, event: AuditEvent) -> AuditEvent {
  process(pipeline, event)
  |> result.unwrap(event)
}

/// Compose two pipelines into one
pub fn compose(a: Pipeline, b: Pipeline) -> Pipeline {
  list.flatten([a, b])
}

/// Create a filter that always continues with the event unchanged
pub fn passthrough() -> Filter {
  fn(event) { Continue(event) }
}

/// Create a filter from a simple transform function
pub fn from_transform(f: fn(AuditEvent) -> AuditEvent) -> Filter {
  fn(event) { Continue(f(event)) }
}

/// Create a filter from a validation function
pub fn from_validator(
  f: fn(AuditEvent) -> Result(Nil, String),
) -> Filter {
  fn(event) {
    case f(event) {
      Ok(Nil) -> Continue(event)
      Error(reason) -> Reject(reason)
    }
  }
}

