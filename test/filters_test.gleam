//// Unit tests for the built-in filters
////
//// Tests validation, normalization, and enrichment filters.

import auditor/entity
import auditor/entity_store
import auditor/event
import auditor/filters
import auditor/pipeline
import gleam/dict
import gleam/option.{None, Some}
import gleeunit/should

// =============================================================================
// Helper Functions
// =============================================================================

fn make_event() -> event.AuditEvent {
  event.new(
    "evt-123",
    "Test@Example.COM",
    "CREATE",
    "document",
    "doc-456",
    "2025-12-17T00:00:00Z",
  )
}

fn make_event_with_entity_key(key: String) -> event.AuditEvent {
  event.new_with_enrichment(
    "evt-123",
    "test@example.com",
    "create",
    "document",
    "doc-456",
    "2025-12-17T00:00:00Z",
    None,
    Some(key),
  )
}

// =============================================================================
// Validation Filter Tests
// =============================================================================

pub fn validate_required_passes_valid_event_test() {
  let evt = make_event()
  let pipe = pipeline.from_filters([filters.validate_required()])

  pipeline.process(pipe, evt)
  |> should.be_ok
}

pub fn validate_required_rejects_empty_actor_test() {
  let evt = event.AuditEvent(..make_event(), actor: "")
  let pipe = pipeline.from_filters([filters.validate_required()])

  pipeline.process(pipe, evt)
  |> should.be_error
  |> should.equal("Rejected: actor is required")
}

pub fn validate_required_rejects_empty_action_test() {
  let evt = event.AuditEvent(..make_event(), action: "")
  let pipe = pipeline.from_filters([filters.validate_required()])

  pipeline.process(pipe, evt)
  |> should.be_error
  |> should.equal("Rejected: action is required")
}

pub fn validate_required_rejects_empty_resource_type_test() {
  let evt = event.AuditEvent(..make_event(), resource_type: "")
  let pipe = pipeline.from_filters([filters.validate_required()])

  pipeline.process(pipe, evt)
  |> should.be_error
  |> should.equal("Rejected: resource_type is required")
}

pub fn validate_actor_email_passes_valid_email_test() {
  let evt = make_event()
  let pipe = pipeline.from_filters([filters.validate_actor_email()])

  pipeline.process(pipe, evt)
  |> should.be_ok
}

pub fn validate_actor_email_rejects_invalid_email_test() {
  let evt = event.AuditEvent(..make_event(), actor: "not-an-email")
  let pipe = pipeline.from_filters([filters.validate_actor_email()])

  pipeline.process(pipe, evt)
  |> should.be_error
  |> should.equal("Rejected: actor must be an email address")
}

// =============================================================================
// Normalization Filter Tests
// =============================================================================

pub fn normalize_actor_lowercases_email_test() {
  let evt = make_event()
  let pipe = pipeline.from_filters([filters.normalize_actor()])

  pipeline.process(pipe, evt)
  |> should.be_ok
  |> fn(result) { result.actor |> should.equal("test@example.com") }
}

pub fn normalize_action_lowercases_action_test() {
  let evt = make_event()
  let pipe = pipeline.from_filters([filters.normalize_action()])

  pipeline.process(pipe, evt)
  |> should.be_ok
  |> fn(result) { result.action |> should.equal("create") }
}

pub fn trim_fields_removes_whitespace_test() {
  let evt =
    event.AuditEvent(
      ..make_event(),
      actor: "  test@example.com  ",
      action: " create ",
      resource_type: "document ",
      resource_id: " doc-123",
    )

  let pipe = pipeline.from_filters([filters.trim_fields()])

  let result = pipeline.process(pipe, evt) |> should.be_ok
  result.actor |> should.equal("test@example.com")
  result.action |> should.equal("create")
  result.resource_type |> should.equal("document")
  result.resource_id |> should.equal("doc-123")
}

// =============================================================================
// Enrichment Filter Tests
// =============================================================================

pub fn add_correlation_id_adds_when_missing_test() {
  let evt = make_event()
  evt.correlation_id |> should.equal(None)

  let pipe = pipeline.from_filters([filters.add_correlation_id()])

  let result = pipeline.process(pipe, evt) |> should.be_ok
  result.correlation_id |> should.not_equal(None)
}

pub fn add_correlation_id_preserves_existing_test() {
  let evt =
    event.new_with_enrichment(
      "evt-123",
      "test@example.com",
      "create",
      "document",
      "doc-456",
      "2025-12-17T00:00:00Z",
      Some("existing-correlation-id"),
      None,
    )

  let pipe = pipeline.from_filters([filters.add_correlation_id()])

  let result = pipeline.process(pipe, evt) |> should.be_ok
  result.correlation_id |> should.equal(Some("existing-correlation-id"))
}

pub fn enrich_from_entity_adds_metadata_test() {
  let store = entity_store.init()

  // Register an entity
  let ent =
    entity.new("org:acme", "Acme Corporation")
    |> entity.with_attribute("tier", "enterprise")
    |> entity.with_attribute("region", "us-west")

  entity_store.put(store, ent)

  // Create event with entity_key
  let evt = make_event_with_entity_key("org:acme")

  let pipe = pipeline.from_filters([filters.enrich_from_entity(store)])

  let result = pipeline.process(pipe, evt) |> should.be_ok

  result.metadata
  |> dict.get("entity_name")
  |> should.equal(Ok("Acme Corporation"))

  result.metadata
  |> dict.get("tier")
  |> should.equal(Ok("enterprise"))

  result.metadata
  |> dict.get("region")
  |> should.equal(Ok("us-west"))
}

pub fn enrich_from_entity_continues_when_not_found_test() {
  let store = entity_store.init()

  // Don't register any entities
  let evt = make_event_with_entity_key("org:nonexistent")

  let pipe = pipeline.from_filters([filters.enrich_from_entity(store)])

  // Should continue without enrichment, not fail
  pipeline.process(pipe, evt)
  |> should.be_ok
}

pub fn enrich_from_entity_skips_when_no_key_test() {
  let store = entity_store.init()
  let evt = make_event()
  evt.entity_key |> should.equal(None)

  let pipe = pipeline.from_filters([filters.enrich_from_entity(store)])

  // Should continue without any changes
  let result = pipeline.process(pipe, evt) |> should.be_ok
  result.metadata |> should.equal(dict.new())
}

pub fn add_metadata_adds_single_key_test() {
  let evt = make_event()

  let pipe = pipeline.from_filters([filters.add_metadata("source", "api")])

  let result = pipeline.process(pipe, evt) |> should.be_ok
  result.metadata
  |> dict.get("source")
  |> should.equal(Ok("api"))
}

pub fn add_source_adds_source_metadata_test() {
  let evt = make_event()

  let pipe = pipeline.from_filters([filters.add_source("chronicle")])

  let result = pipeline.process(pipe, evt) |> should.be_ok
  result.metadata
  |> dict.get("source")
  |> should.equal(Ok("chronicle"))
}

// =============================================================================
// Default Pipeline Tests
// =============================================================================

pub fn default_pipeline_processes_event_test() {
  let evt =
    event.AuditEvent(
      ..make_event(),
      actor: "  Alice@EXAMPLE.COM  ",
      action: "  CREATE  ",
    )

  let pipe = filters.default_pipeline()

  let result = pipeline.process(pipe, evt) |> should.be_ok

  // Should be trimmed and normalized
  result.actor |> should.equal("alice@example.com")
  result.action |> should.equal("CREATE")
  // action trimmed but not lowercased

  // Should have correlation ID
  result.correlation_id |> should.not_equal(None)
}

pub fn enrichment_pipeline_includes_entity_lookup_test() {
  let store = entity_store.init()

  let ent =
    entity.new("project:alpha", "Project Alpha")
    |> entity.with_attribute("team", "platform")

  entity_store.put(store, ent)

  let evt = make_event_with_entity_key("project:alpha")

  let pipe = filters.enrichment_pipeline(store)

  let result = pipeline.process(pipe, evt) |> should.be_ok

  result.metadata
  |> dict.get("entity_name")
  |> should.equal(Ok("Project Alpha"))

  result.metadata
  |> dict.get("team")
  |> should.equal(Ok("platform"))
}

