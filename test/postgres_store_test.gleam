//// Integration tests for PostgreSQL Event Store
////
//// These tests require a running PostgreSQL instance.
//// Run with: just pg-up && just migrate && gleam test

import auditor/event
import auditor/event_store
import gleam/list
import gleam/option.{Some}
import gleeunit/should

// =============================================================================
// Test Configuration
// =============================================================================

fn test_pg_config() -> event_store.PostgresConfig {
  event_store.PostgresConfig(
    host: "localhost",
    port: 5432,
    database: "chronicle",
    user: "chronicle",
    password: "chronicle",
  )
}

fn make_test_event(id: String, action: String) -> event.AuditEvent {
  event.new(
    id,
    "test@example.com",
    action,
    "test_resource",
    "test-123",
    "2025-12-20T12:00:00.000Z",
  )
}

// =============================================================================
// PostgreSQL Backend Tests
// =============================================================================

pub fn postgres_insert_and_get_test() {
  let store = event_store.create_postgres(test_pg_config())
  
  let test_id = "pg-test-" <> random_suffix()
  let evt = make_test_event(test_id, "create")
  
  // Insert should succeed
  event_store.insert(store, evt)
  |> should.be_true
  
  // Get should find the event
  let result = event_store.get(store, test_id)
  result |> should.be_ok
  
  let assert Ok(found) = result
  found.id |> should.equal(test_id)
  found.actor |> should.equal("test@example.com")
  found.action |> should.equal("create")
}

pub fn postgres_list_all_test() {
  let store = event_store.create_postgres(test_pg_config())
  
  // Insert a few events
  let test_id1 = "pg-list-" <> random_suffix() <> "-1"
  let test_id2 = "pg-list-" <> random_suffix() <> "-2"
  
  event_store.insert(store, make_test_event(test_id1, "create"))
  |> should.be_true
  
  event_store.insert(store, make_test_event(test_id2, "update"))
  |> should.be_true
  
  // List should include our events
  let events = event_store.list_all(store)
  
  let ids = list.map(events, fn(e) { e.id })
  list.contains(ids, test_id1) |> should.be_true
  list.contains(ids, test_id2) |> should.be_true
}

pub fn postgres_get_missing_returns_error_test() {
  let store = event_store.create_postgres(test_pg_config())
  
  event_store.get(store, "nonexistent-event-id-12345")
  |> should.be_error
}

pub fn postgres_duplicate_insert_is_idempotent_test() {
  let store = event_store.create_postgres(test_pg_config())
  
  let test_id = "pg-dup-" <> random_suffix()
  let evt = make_test_event(test_id, "create")
  
  // First insert
  event_store.insert(store, evt) |> should.be_true
  
  // Second insert with same ID should also succeed (ON CONFLICT DO NOTHING)
  event_store.insert(store, evt) |> should.be_true
  
  // Should still only have one event with this ID
  event_store.get(store, test_id) |> should.be_ok
}

pub fn postgres_preserves_all_fields_test() {
  let store = event_store.create_postgres(test_pg_config())

  let test_id = "pg-fields-" <> random_suffix()
  // Use the new_with_type constructor and add enrichment fields
  let original =
    event.new_with_type(
      test_id,
      "alice@company.com",
      "archive",
      "document",
      "doc-789",
      "2025-12-20T15:30:00.000Z",
      "document.archive",
    )
    |> fn(evt) {
      event.AuditEvent(
        ..evt,
        correlation_id: Some("corr-123"),
        entity_key: Some("org:acme"),
      )
    }

  event_store.insert(store, original)
  |> should.be_true

  let assert Ok(retrieved) = event_store.get(store, test_id)

  retrieved.id
  |> should.equal(test_id)
  retrieved.actor
  |> should.equal("alice@company.com")
  retrieved.action
  |> should.equal("archive")
  retrieved.resource_type
  |> should.equal("document")
  retrieved.resource_id
  |> should.equal("doc-789")
  retrieved.event_type
  |> should.equal(Some("document.archive"))
  retrieved.correlation_id
  |> should.equal(Some("corr-123"))
  retrieved.entity_key
  |> should.equal(Some("org:acme"))
}

// =============================================================================
// Helpers
// =============================================================================

@external(erlang, "erlang", "unique_integer")
fn unique_int() -> Int

fn random_suffix() -> String {
  int_to_string(unique_int())
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(n: Int) -> String

