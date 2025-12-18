//// Filters - built-in filters for the Pipes and Filters pattern
////
//// Filters are just functions that transform events! Compose them
//// with Gleam's pipe operator for clean, readable pipelines.
////
//// Example:
////   Ok(event)
////   |> result.try(validate_required)
////   |> result.try(trim_fields)
////   |> result.try(normalize_actor)
////   |> result.try(add_correlation_id)
////
//// NOTE: Entity enrichment happens at read time, not in the ingestion pipeline.
//// This ensures events always reflect current entity data.

import auditor/entity_store.{type EntityTable}
import auditor/event.{type AuditEvent}
import auditor/log
import auditor/pipeline
import gleam/dict
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import youid/uuid

// =============================================================================
// Validation Filters
// =============================================================================

/// Validate that required fields are present and non-empty
pub fn validate_required(event: AuditEvent) -> Result(AuditEvent, String) {
  case event.actor, event.action, event.resource_type {
    "", _, _ -> Error("actor is required")
    _, "", _ -> Error("action is required")
    _, _, "" -> Error("resource_type is required")
    _, _, _ -> Ok(event)
  }
}

/// Validate actor looks like an email (contains @)
pub fn validate_actor_email(event: AuditEvent) -> Result(AuditEvent, String) {
  case string.contains(event.actor, "@") {
    True -> Ok(event)
    False -> Error("actor must be an email address")
  }
}

// =============================================================================
// Normalization Filters
// =============================================================================

/// Normalize actor to lowercase (for consistent lookups)
pub fn normalize_actor(event: AuditEvent) -> Result(AuditEvent, String) {
  Ok(event.AuditEvent(..event, actor: string.lowercase(event.actor)))
}

/// Normalize action to lowercase
pub fn normalize_action(event: AuditEvent) -> Result(AuditEvent, String) {
  Ok(event.AuditEvent(..event, action: string.lowercase(event.action)))
}

/// Trim whitespace from all string fields
pub fn trim_fields(event: AuditEvent) -> Result(AuditEvent, String) {
  Ok(
    event.AuditEvent(
      ..event,
      actor: string.trim(event.actor),
      action: string.trim(event.action),
      resource_type: string.trim(event.resource_type),
      resource_id: string.trim(event.resource_id),
    ),
  )
}

// =============================================================================
// Enrichment Filters
// =============================================================================

/// Add a correlation ID if not already present
pub fn add_correlation_id(event: AuditEvent) -> Result(AuditEvent, String) {
  case event.correlation_id {
    Some(_) -> Ok(event)
    None -> {
      let cid = uuid.v4_string()
      Ok(event.AuditEvent(..event, correlation_id: Some(cid)))
    }
  }
}

/// Enrich event from entity registry based on entity_key
/// NOTE: For most use cases, prefer read-time enrichment (see router.hydrate_event)
/// This filter is provided for custom pipelines where write-time enrichment is desired
pub fn enrich_from_entity(
  store: EntityTable,
) -> fn(AuditEvent) -> Result(AuditEvent, String) {
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
            Ok(event.AuditEvent(..event, metadata: new_metadata))
          }
          Error(Nil) -> {
            // Entity not found - continue without enrichment
            log.warn("Entity not found for enrichment: " <> key)
            Ok(event)
          }
        }
      }
      None -> Ok(event)
    }
  }
}

/// Add static metadata to all events
pub fn add_metadata(
  key: String,
  value: String,
) -> fn(AuditEvent) -> Result(AuditEvent, String) {
  fn(evt: AuditEvent) { Ok(event.with_metadata(evt, key, value)) }
}

/// Add source system identifier
pub fn add_source(
  source: String,
) -> fn(AuditEvent) -> Result(AuditEvent, String) {
  add_metadata("source", source)
}

// =============================================================================
// Logging Filters
// =============================================================================

/// Log the event passing through (passthrough - doesn't modify event)
pub fn log_event(prefix: String) -> fn(AuditEvent) -> Result(AuditEvent, String) {
  fn(event: AuditEvent) {
    log.info(
      prefix <> " " <> event.id <> " " <> event.actor <> " " <> event.action,
    )
    Ok(event)
  }
}

/// Log event with debug level
pub fn log_debug(prefix: String) -> fn(AuditEvent) -> Result(AuditEvent, String) {
  fn(event: AuditEvent) {
    log.debug(
      prefix
      <> " id="
      <> event.id
      <> " actor="
      <> event.actor
      <> " action="
      <> event.action,
    )
    Ok(event)
  }
}

// =============================================================================
// Pipeline helpers
// =============================================================================

/// Run the standard ingestion pipeline on an event
/// Validates, trims, normalizes, and adds correlation ID
pub fn ingest(event: AuditEvent) -> Result(AuditEvent, String) {
  Ok(event)
  |> result.try(validate_required)
  |> result.try(trim_fields)
  |> result.try(normalize_actor)
  |> result.try(add_correlation_id)
}

/// Run the ingestion pipeline using the list-based runner
/// (for when you need dynamic filter composition)
pub fn ingestion_filters() -> List(pipeline.Filter) {
  [validate_required, trim_fields, normalize_actor, add_correlation_id]
}
