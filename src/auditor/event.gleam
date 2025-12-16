/// Audit event type - represents something that happened in the system
import gleam/json.{type Json}
import gleam/option.{type Option, None}

/// An audit event capturing who did what to which resource
pub type AuditEvent {
  AuditEvent(
    id: String,
    actor: String,
    action: String,
    resource_type: String,
    resource_id: String,
    timestamp: String,
    metadata: Option(String),
  )
}

/// Encode an audit event to JSON
pub fn to_json(event: AuditEvent) -> Json {
  json.object([
    #("id", json.string(event.id)),
    #("actor", json.string(event.actor)),
    #("action", json.string(event.action)),
    #("resource_type", json.string(event.resource_type)),
    #("resource_id", json.string(event.resource_id)),
    #("timestamp", json.string(event.timestamp)),
  ])
}

/// Create an audit event with required fields
pub fn new(
  id: String,
  actor: String,
  action: String,
  resource_type: String,
  resource_id: String,
  timestamp: String,
) -> AuditEvent {
  AuditEvent(
    id: id,
    actor: actor,
    action: action,
    resource_type: resource_type,
    resource_id: resource_id,
    timestamp: timestamp,
    metadata: None,
  )
}
