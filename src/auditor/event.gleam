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
pub fn to_json(evt: AuditEvent) -> Json {
  // Build optional fields list
  let optional_fields =
    []
    |> add_optional_field("correlation_id", evt.correlation_id)
    |> add_optional_field("entity_key", evt.entity_key)
    |> add_metadata_field(evt.metadata)

  // Combine base fields with optional fields
  json.object(
    list.flatten([
      [
        #("id", json.string(evt.id)),
        #("actor", json.string(evt.actor)),
        #("action", json.string(evt.action)),
        #("resource_type", json.string(evt.resource_type)),
        #("resource_id", json.string(evt.resource_id)),
        #("timestamp", json.string(evt.timestamp)),
      ],
      optional_fields,
    ]),
  )
}

fn add_optional_field(
  fields: List(#(String, Json)),
  key: String,
  value: Option(String),
) -> List(#(String, Json)) {
  case value {
    Some(v) -> [#(key, json.string(v)), ..fields]
    None -> fields
  }
}

fn add_metadata_field(
  fields: List(#(String, Json)),
  metadata: Dict(String, String),
) -> List(#(String, Json)) {
  case dict.is_empty(metadata) {
    True -> fields
    False -> {
      let metadata_json =
        metadata
        |> dict.to_list
        |> list.map(fn(pair) { #(pair.0, json.string(pair.1)) })
        |> json.object
      [#("metadata", metadata_json), ..fields]
    }
  }
}
