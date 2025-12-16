/// ETS-based event store - simple in-memory persistence
import auditor/event.{type AuditEvent}

/// The ETS table reference type
pub type Table

/// Initialize the ETS table for storing events
@external(erlang, "auditor_store_ffi", "init")
pub fn init() -> Table

/// Store an event in ETS
@external(erlang, "auditor_store_ffi", "insert")
pub fn insert(table: Table, event: AuditEvent) -> Bool

/// List all events from ETS
@external(erlang, "auditor_store_ffi", "list_all")
pub fn list_all(table: Table) -> List(AuditEvent)

/// Get an event by ID
@external(erlang, "auditor_store_ffi", "get")
pub fn get(table: Table, id: String) -> Result(AuditEvent, Nil)
