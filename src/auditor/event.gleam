//// Audit Event - the core message type
////
//// Represents an action performed by an actor on a resource.
//// This is the "Message" in EIP terminology.
////
//// Events can be enriched via the Pipes and Filters pattern:
//// - correlation_id: links related events for tracing
//// - entity_key: references an entity for metadata enrichment
//// - metadata: extensible key-value pairs added by filters

import gleam/dict.{type Dict}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}

/// An audit event capturing who did what to which resource
pub type AuditEvent {
  AuditEvent(
    id: String,
    actor: String,
    action: String,
    resource_type: String,
    resource_id: String,
    timestamp: String,
    // Enrichment fields
    correlation_id: Option(String),
    entity_key: Option(String),
    metadata: Dict(String, String),
  )
}

/// Create a new audit event with default enrichment fields
pub fn new(
  id: String,
  actor: String,
  action: String,
  resource_type: String,
  resource_id: String,
  timestamp: String,
) -> AuditEvent {
  AuditEvent(
    id:,
    actor:,
    action:,
    resource_type:,
    resource_id:,
    timestamp:,
    correlation_id: None,
    entity_key: None,
    metadata: dict.new(),
  )
}

/// Create a new audit event with enrichment fields
pub fn new_with_enrichment(
  id: String,
  actor: String,
  action: String,
  resource_type: String,
  resource_id: String,
  timestamp: String,
  correlation_id: Option(String),
  entity_key: Option(String),
) -> AuditEvent {
  AuditEvent(
    id:,
    actor:,
    action:,
    resource_type:,
    resource_id:,
    timestamp:,
    correlation_id:,
    entity_key:,
    metadata: dict.new(),
  )
}

/// Add metadata to an event
pub fn with_metadata(event: AuditEvent, key: String, value: String) -> AuditEvent {
  AuditEvent(..event, metadata: dict.insert(event.metadata, key, value))
}

/// Merge metadata into an event
pub fn merge_metadata(
  event: AuditEvent,
  new_metadata: Dict(String, String),
) -> AuditEvent {
  AuditEvent(..event, metadata: dict.merge(event.metadata, new_metadata))
}

/// Encode an audit event to JSON
pub fn to_json(event: AuditEvent) -> Json {
  let base = [
    #("id", json.string(event.id)),
    #("actor", json.string(event.actor)),
    #("action", json.string(event.action)),
    #("resource_type", json.string(event.resource_type)),
    #("resource_id", json.string(event.resource_id)),
    #("timestamp", json.string(event.timestamp)),
  ]

  // Add optional fields if present
  let with_correlation = case event.correlation_id {
    Some(cid) -> list.append(base, [#("correlation_id", json.string(cid))])
    None -> base
  }

  let with_entity = case event.entity_key {
    Some(ek) -> list.append(with_correlation, [#("entity_key", json.string(ek))])
    None -> with_correlation
  }

  // Add metadata if non-empty
  let with_metadata = case dict.is_empty(event.metadata) {
    True -> with_entity
    False -> {
      let metadata_json =
        event.metadata
        |> dict.to_list
        |> list.map(fn(pair) { #(pair.0, json.string(pair.1)) })
        |> json.object
      list.append(with_entity, [#("metadata", metadata_json)])
    }
  }

  json.object(with_metadata)
}
