//// Audit Event - the core message type
////
//// Represents an action performed by an actor on a resource.
//// This is the "Message" in EIP terminology.

import gleam/json.{type Json}

/// An audit event capturing who did what to which resource
pub type AuditEvent {
  AuditEvent(
    id: String,
    actor: String,
    action: String,
    resource_type: String,
    resource_id: String,
    timestamp: String,
  )
}

/// Create a new audit event
pub fn new(
  id: String,
  actor: String,
  action: String,
  resource_type: String,
  resource_id: String,
  timestamp: String,
) -> AuditEvent {
  AuditEvent(id:, actor:, action:, resource_type:, resource_id:, timestamp:)
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
