//// Pipeline - Pipes and Filters pattern implementation
////
//// From Enterprise Integration Patterns:
//// > "Divide a larger processing task into a sequence of smaller,
//// > independent processing steps (Filters) that are connected by
//// > channels (Pipes)."
////
//// Filters are just functions! Chain them with Gleam's pipe operator
//// and result.try for clean, composable event processing.
////
//// Example:
////   Ok(event)
////   |> result.try(validate_required)
////   |> result.try(trim_fields)
////   |> result.try(normalize_actor)
////   |> result.try(add_correlation_id)

import auditor/event.{type AuditEvent}
import gleam/result

/// A Filter is simply a function that transforms an event or returns an error
pub type Filter =
  fn(AuditEvent) -> Result(AuditEvent, String)

/// Process an event through a chain of filters
/// This is just for convenience - you can also pipe directly with result.try
pub fn run(
  event: AuditEvent,
  filters: List(Filter),
) -> Result(AuditEvent, String) {
  case filters {
    [] -> Ok(event)
    [filter, ..rest] -> {
      filter(event)
      |> result.try(fn(e) { run(e, rest) })
    }
  }
}

/// Process an event, returning the original on error (lenient mode)
pub fn run_lenient(event: AuditEvent, filters: List(Filter)) -> AuditEvent {
  run(event, filters)
  |> result.unwrap(event)
}

