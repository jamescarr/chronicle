//// Filters - built-in filters for the Pipes and Filters pattern
////
//// These filters can be composed into pipelines to process audit events.
//// Each filter is a function that transforms or validates an event.
////
//// Example pipeline:
////   pipeline.from_filters([
////     filters.validate_required(),
////     filters.normalize_actor(),
////     filters.add_correlation_id(),
////     filters.enrich_from_entity(entity_store),
////     filters.log_event("[pipeline]"),
////   ])

import auditor/entity_store.{type EntityTable}
import auditor/event.{type AuditEvent}
import auditor/log
import auditor/pipeline.{type Filter, Continue, Reject}
import gleam/dict
import gleam/option.{None, Some}
import gleam/string
import youid/uuid

// =============================================================================
// Validation Filters
// =============================================================================

/// Validate that required fields are present and non-empty
pub fn validate_required() -> Filter {
  fn(event: AuditEvent) {
    case event.actor, event.action, event.resource_type {
      "", _, _ -> Reject("actor is required")
      _, "", _ -> Reject("action is required")
      _, _, "" -> Reject("resource_type is required")
      _, _, _ -> Continue(event)
    }
  }
}

/// Validate actor looks like an email (contains @)
pub fn validate_actor_email() -> Filter {
  fn(event: AuditEvent) {
    case string.contains(event.actor, "@") {
      True -> Continue(event)
      False -> Reject("actor must be an email address")
    }
  }
}

// =============================================================================
// Normalization Filters
// =============================================================================

/// Normalize actor to lowercase (for consistent lookups)
pub fn normalize_actor() -> Filter {
  fn(event: AuditEvent) {
    Continue(event.AuditEvent(..event, actor: string.lowercase(event.actor)))
  }
}

/// Normalize action to lowercase
pub fn normalize_action() -> Filter {
  fn(event: AuditEvent) {
    Continue(event.AuditEvent(..event, action: string.lowercase(event.action)))
  }
}

/// Trim whitespace from all string fields
pub fn trim_fields() -> Filter {
  fn(event: AuditEvent) {
    Continue(
      event.AuditEvent(
        ..event,
        actor: string.trim(event.actor),
        action: string.trim(event.action),
        resource_type: string.trim(event.resource_type),
        resource_id: string.trim(event.resource_id),
      ),
    )
  }
}

// =============================================================================
// Enrichment Filters
// =============================================================================

/// Add a correlation ID if not already present
pub fn add_correlation_id() -> Filter {
  fn(event: AuditEvent) {
    case event.correlation_id {
      Some(_) -> Continue(event)
      None -> {
        let cid = uuid.v4_string()
        Continue(event.AuditEvent(..event, correlation_id: Some(cid)))
      }
    }
  }
}

/// Enrich event from entity registry based on entity_key
pub fn enrich_from_entity(store: EntityTable) -> Filter {
  fn(event: AuditEvent) {
    case event.entity_key {
      Some(key) -> {
        case entity_store.get(store, key) {
          Ok(entity) -> {
            // Merge entity attributes into event metadata
            let new_metadata =
              event.metadata
              |> dict.insert("entity_name", entity.name)
              |> dict.merge(entity.attributes)
            Continue(event.AuditEvent(..event, metadata: new_metadata))
          }
          Error(Nil) -> {
            // Entity not found - continue without enrichment
            log.warn("Entity not found for enrichment: " <> key)
            Continue(event)
          }
        }
      }
      None -> Continue(event)
    }
  }
}

/// Add static metadata to all events
pub fn add_metadata(key: String, value: String) -> Filter {
  fn(event: AuditEvent) {
    Continue(event.with_metadata(event, key, value))
  }
}

/// Add source system identifier
pub fn add_source(source: String) -> Filter {
  add_metadata("source", source)
}

// =============================================================================
// Logging Filters
// =============================================================================

/// Log the event passing through (passthrough - doesn't modify event)
pub fn log_event(prefix: String) -> Filter {
  fn(event: AuditEvent) {
    log.info(prefix <> " " <> event.id <> " " <> event.actor <> " " <> event.action)
    Continue(event)
  }
}

/// Log event with debug level
pub fn log_debug(prefix: String) -> Filter {
  fn(event: AuditEvent) {
    log.debug(
      prefix <> " id=" <> event.id <> " actor=" <> event.actor <> " action="
      <> event.action,
    )
    Continue(event)
  }
}

// =============================================================================
// Default Pipelines
// =============================================================================

/// Default pipeline for processing events
/// Validates, normalizes, and adds correlation ID
pub fn default_pipeline() -> pipeline.Pipeline {
  pipeline.from_filters([
    validate_required(),
    trim_fields(),
    normalize_actor(),
    add_correlation_id(),
  ])
}

/// Pipeline with entity enrichment
pub fn enrichment_pipeline(store: EntityTable) -> pipeline.Pipeline {
  pipeline.from_filters([
    validate_required(),
    trim_fields(),
    normalize_actor(),
    add_correlation_id(),
    enrich_from_entity(store),
  ])
}

