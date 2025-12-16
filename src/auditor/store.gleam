//// ETS-based event store
////
//// Simple in-memory persistence using Erlang Term Storage.
//// Events are stored in a set keyed by their ID.

import auditor/event.{type AuditEvent}

/// Opaque ETS table reference
pub type Table

/// Initialize a new ETS table
@external(erlang, "auditor_store_ffi", "init")
pub fn init() -> Table

/// Insert an event into the store
@external(erlang, "auditor_store_ffi", "insert")
pub fn insert(table: Table, event: AuditEvent) -> Bool

/// List all events in the store
@external(erlang, "auditor_store_ffi", "list_all")
pub fn list_all(table: Table) -> List(AuditEvent)

/// Get an event by ID
@external(erlang, "auditor_store_ffi", "get")
pub fn get(table: Table, id: String) -> Result(AuditEvent, Nil)
