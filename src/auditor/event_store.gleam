//// Event Store - Abstraction for event persistence
////
//// Provides a pluggable storage backend for audit events.
//// Consumers use this interface without knowing if events
//// are stored in ETS, PostgreSQL, or any other backend.
////
//// Usage:
////   let store = event_store.create_ets()  // or create_postgres(config)
////   event_store.insert(store, event)
////   let events = event_store.list_all(store)

import auditor/event.{type AuditEvent}

// =============================================================================
// Store Interface
// =============================================================================

/// Event store - a record of functions that any backend must implement
/// This is the "interface" that allows swapping implementations
pub type EventStore {
  EventStore(
    insert: fn(AuditEvent) -> Bool,
    list_all: fn() -> List(AuditEvent),
    get: fn(String) -> Result(AuditEvent, Nil),
  )
}

// =============================================================================
// Store Operations
// =============================================================================

/// Insert an event into the store
pub fn insert(store: EventStore, event: AuditEvent) -> Bool {
  store.insert(event)
}

/// List all events in the store
pub fn list_all(store: EventStore) -> List(AuditEvent) {
  store.list_all()
}

/// Get an event by ID
pub fn get(store: EventStore, id: String) -> Result(AuditEvent, Nil) {
  store.get(id)
}

// =============================================================================
// ETS Backend
// =============================================================================

/// Opaque ETS table reference
pub type EtsTable

/// Create an ETS-backed event store (in-memory)
pub fn create_ets() -> EventStore {
  let table = ets_init()

  EventStore(
    insert: fn(event) { ets_insert(table, event) },
    list_all: fn() { ets_list_all(table) },
    get: fn(id) { ets_get(table, id) },
  )
}

@external(erlang, "auditor_store_ffi", "init")
fn ets_init() -> EtsTable

@external(erlang, "auditor_store_ffi", "insert")
fn ets_insert(table: EtsTable, event: AuditEvent) -> Bool

@external(erlang, "auditor_store_ffi", "list_all")
fn ets_list_all(table: EtsTable) -> List(AuditEvent)

@external(erlang, "auditor_store_ffi", "get")
fn ets_get(table: EtsTable, id: String) -> Result(AuditEvent, Nil)

// =============================================================================
// PostgreSQL Backend (placeholder - implement when ready)
// =============================================================================

/// PostgreSQL connection configuration
pub type PostgresConfig {
  PostgresConfig(
    host: String,
    port: Int,
    database: String,
    user: String,
    password: String,
  )
}

/// Create a PostgreSQL-backed event store
/// TODO: Implement actual PostgreSQL connection
pub fn create_postgres(_config: PostgresConfig) -> EventStore {
  // For now, fall back to ETS - replace with real postgres when ready
  create_ets()
}
