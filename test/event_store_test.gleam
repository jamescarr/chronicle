//// Unit tests for the Event Store abstraction
////
//// Tests the pluggable storage backend for audit events.

import auditor/event
import auditor/event_store
import gleeunit/should

// =============================================================================
// Helper Functions
// =============================================================================

fn make_event(id: String, action: String) -> event.AuditEvent {
  event.new(id, "test@example.com", action, "test", "test-123", "2025-12-16T00:00:00Z")
}

// =============================================================================
// ETS Backend Tests
// =============================================================================

pub fn create_ets_store_test() {
  // Should be able to create an ETS store
  let _store = event_store.create_ets()
  // No assertion needed - if it doesn't crash, it works
}

pub fn insert_and_list_test() {
  let store = event_store.create_ets()
  
  // Insert events
  event_store.insert(store, make_event("evt-1", "create"))
  |> should.be_true
  
  event_store.insert(store, make_event("evt-2", "update"))
  |> should.be_true
  
  // List should return both events
  let events = event_store.list_all(store)
  events |> should.not_equal([])
}

pub fn get_by_id_test() {
  let store = event_store.create_ets()
  
  let evt = make_event("test-id-123", "create")
  event_store.insert(store, evt)
  
  // Should find the event
  let result = event_store.get(store, "test-id-123")
  result |> should.be_ok
  
  let assert Ok(found) = result
  found.id |> should.equal("test-id-123")
  found.action |> should.equal("create")
}

pub fn get_missing_returns_error_test() {
  let store = event_store.create_ets()
  
  // Should return error for missing event
  event_store.get(store, "nonexistent")
  |> should.be_error
}

pub fn preserves_event_data_test() {
  let store = event_store.create_ets()
  
  let original = event.new(
    "test-id-456",
    "alice@company.com",
    "archive",
    "document",
    "doc-789",
    "2025-12-17T12:00:00Z",
  )
  
  event_store.insert(store, original)
  
  let assert Ok(retrieved) = event_store.get(store, "test-id-456")
  
  retrieved.id |> should.equal("test-id-456")
  retrieved.actor |> should.equal("alice@company.com")
  retrieved.action |> should.equal("archive")
  retrieved.resource_type |> should.equal("document")
  retrieved.resource_id |> should.equal("doc-789")
  retrieved.timestamp |> should.equal("2025-12-17T12:00:00Z")
}

pub fn multiple_stores_are_isolated_test() {
  let store1 = event_store.create_ets()
  let store2 = event_store.create_ets()
  
  // Insert into store1 only
  event_store.insert(store1, make_event("store1-event", "create"))
  
  // store2 should be empty
  event_store.get(store2, "store1-event")
  |> should.be_error
}

