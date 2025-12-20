//// Audit Event - the core message type
////
//// Represents an action performed by an actor on a resource.
//// This is the "Message" in EIP terminology.
////
//// Events can be enriched via the Pipes and Filters pattern:
//// - correlation_id: links related events for tracing
//// - entity_key: references an entity for metadata enrichment
//// - metadata: extensible key-value pairs added by filters
////
//// The event_type field enables Datatype Channel routing:
//// - Format: "category.action" (e.g., "security.login", "user.created")
//// - Used as RabbitMQ routing key for topic exchanges

import gleam/dict.{type Dict}
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

/// An audit event capturing who did what to which resource
pub type AuditEvent {
  AuditEvent(
    id: String,
    actor: String,
    action: String,
    resource_type: String,
    resource_id: String,
    timestamp: String,
    // Datatype Channel routing key
    event_type: Option(String),
    // Enrichment fields
    correlation_id: Option(String),
    entity_key: Option(String),
    metadata: Dict(String, String),
  )
}

/// Create a new audit event with default enrichment fields
/// The event_type is auto-derived from resource_type.action
pub fn new(
  id: String,
  actor: String,
  action: String,
  resource_type: String,
  resource_id: String,
  timestamp: String,
) -> AuditEvent {
  let event_type = derive_event_type(resource_type, action)
  AuditEvent(
    id:,
    actor:,
    action:,
    resource_type:,
    resource_id:,
    timestamp:,
    event_type: Some(event_type),
    correlation_id: None,
    entity_key: None,
    metadata: dict.new(),
  )
}

/// Derive event_type from resource_type and action
/// e.g., ("User", "Login") -> "user.login"
fn derive_event_type(resource_type: String, action: String) -> String {
  string.lowercase(resource_type) <> "." <> string.lowercase(action)
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
  let event_type = derive_event_type(resource_type, action)
  AuditEvent(
    id:,
    actor:,
    action:,
    resource_type:,
    resource_id:,
    timestamp:,
    event_type: Some(event_type),
    correlation_id:,
    entity_key:,
    metadata: dict.new(),
  )
}

/// Create an event with explicit event_type (for custom routing)
pub fn new_with_type(
  id: String,
  actor: String,
  action: String,
  resource_type: String,
  resource_id: String,
  timestamp: String,
  event_type: String,
) -> AuditEvent {
  AuditEvent(
    id:,
    actor:,
    action:,
    resource_type:,
    resource_id:,
    timestamp:,
    event_type: Some(event_type),
    correlation_id: None,
    entity_key: None,
    metadata: dict.new(),
  )
}

/// Create an event with all optional fields
/// Used when receiving events via API with explicit event_type for routing
pub fn new_with_all_options(
  id: String,
  actor: String,
  action: String,
  resource_type: String,
  resource_id: String,
  timestamp: String,
  correlation_id: Option(String),
  entity_key: Option(String),
  event_type: Option(String),
) -> AuditEvent {
  // Use provided event_type or derive from resource_type.action
  let final_event_type = case event_type {
    Some(et) -> et
    None -> derive_event_type(resource_type, action)
  }
  AuditEvent(
    id:,
    actor:,
    action:,
    resource_type:,
    resource_id:,
    timestamp:,
    event_type: Some(final_event_type),
    correlation_id:,
    entity_key:,
    metadata: dict.new(),
  )
}

/// Get the routing key for this event
/// Returns the event_type or a default based on resource_type.action
pub fn routing_key(event: AuditEvent) -> String {
  case event.event_type {
    Some(et) -> et
    None -> derive_event_type(event.resource_type, event.action)
  }
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
    |> add_optional_field("event_type", evt.event_type)
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
