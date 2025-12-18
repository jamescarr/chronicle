//// Unit tests for the Entity Store
////
//// Tests the ETS-backed entity registry used for enrichment lookups.

import auditor/entity
import auditor/entity_store
import gleam/dict
import gleeunit/should

// =============================================================================
// Basic CRUD Tests
// =============================================================================

pub fn entity_store_init_test() {
  // Should be able to initialize a store
  let _table = entity_store.init()
  // No assertion needed - if it doesn't crash, it works
}

pub fn entity_store_put_and_get_test() {
  let table = entity_store.init()

  let ent = entity.new("org:acme", "Acme Corporation")

  entity_store.put(table, ent)
  |> should.be_true

  let result = entity_store.get(table, "org:acme")
  result |> should.be_ok

  let assert Ok(retrieved) = result
  retrieved.key |> should.equal("org:acme")
  retrieved.name |> should.equal("Acme Corporation")
}

pub fn entity_store_get_missing_test() {
  let table = entity_store.init()

  entity_store.get(table, "nonexistent")
  |> should.be_error
}

pub fn entity_store_delete_test() {
  let table = entity_store.init()

  let ent = entity.new("org:delete-me", "To Be Deleted")
  entity_store.put(table, ent)

  // Verify it exists
  entity_store.exists(table, "org:delete-me")
  |> should.be_true

  // Delete it
  entity_store.delete(table, "org:delete-me")
  |> should.be_true

  // Verify it's gone
  entity_store.exists(table, "org:delete-me")
  |> should.be_false
}

pub fn entity_store_list_all_test() {
  let table = entity_store.init()

  entity_store.put(table, entity.new("org:alpha", "Alpha Inc"))
  entity_store.put(table, entity.new("org:beta", "Beta Corp"))
  entity_store.put(table, entity.new("org:gamma", "Gamma LLC"))

  let entities = entity_store.list_all(table)

  entities
  |> should.not_equal([])
}

pub fn entity_store_count_test() {
  let table = entity_store.init()

  entity_store.count(table)
  |> should.equal(0)

  entity_store.put(table, entity.new("org:one", "One"))
  entity_store.count(table)
  |> should.equal(1)

  entity_store.put(table, entity.new("org:two", "Two"))
  entity_store.count(table)
  |> should.equal(2)
}

pub fn entity_store_update_existing_test() {
  let table = entity_store.init()

  // Insert initial entity
  let ent1 = entity.new("org:update", "Original Name")
  entity_store.put(table, ent1)

  // Update with new name
  let ent2 = entity.new("org:update", "Updated Name")
  entity_store.put(table, ent2)

  // Should have the updated name
  let assert Ok(retrieved) = entity_store.get(table, "org:update")
  retrieved.name |> should.equal("Updated Name")

  // Count should still be 1
  entity_store.count(table)
  |> should.equal(1)
}

// =============================================================================
// Entity with Attributes Tests
// =============================================================================

pub fn entity_with_attributes_test() {
  let table = entity_store.init()

  let attrs =
    dict.new()
    |> dict.insert("tier", "enterprise")
    |> dict.insert("region", "us-west")

  let ent = entity.new_with_attributes("org:acme", "Acme Corp", attrs)
  entity_store.put(table, ent)

  let assert Ok(retrieved) = entity_store.get(table, "org:acme")

  retrieved.attributes
  |> dict.get("tier")
  |> should.equal(Ok("enterprise"))

  retrieved.attributes
  |> dict.get("region")
  |> should.equal(Ok("us-west"))
}

pub fn entity_builder_pattern_test() {
  let ent =
    entity.new("project:alpha", "Project Alpha")
    |> entity.with_attribute("status", "active")
    |> entity.with_attribute("team", "platform")
    |> entity.with_attribute("priority", "high")

  ent.key |> should.equal("project:alpha")
  ent.name |> should.equal("Project Alpha")

  ent.attributes
  |> dict.get("status")
  |> should.equal(Ok("active"))

  ent.attributes
  |> dict.get("team")
  |> should.equal(Ok("platform"))

  ent.attributes
  |> dict.get("priority")
  |> should.equal(Ok("high"))
}

