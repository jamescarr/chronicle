//// Entity Store - ETS-backed registry for enrichment entities
////
//// Provides CRUD operations for entities used in the Pipes and Filters
//// enrichment process. Entities are stored in ETS for fast lookups.

import auditor/entity.{type Entity}

/// Opaque ETS table reference for entities
pub type EntityTable

/// Initialize a new entity store
@external(erlang, "auditor_entity_store_ffi", "init")
pub fn init() -> EntityTable

/// Insert or update an entity
@external(erlang, "auditor_entity_store_ffi", "put")
pub fn put(table: EntityTable, entity: Entity) -> Bool

/// Get an entity by key
@external(erlang, "auditor_entity_store_ffi", "get")
pub fn get(table: EntityTable, key: String) -> Result(Entity, Nil)

/// Delete an entity by key
@external(erlang, "auditor_entity_store_ffi", "delete")
pub fn delete(table: EntityTable, key: String) -> Bool

/// List all entities
@external(erlang, "auditor_entity_store_ffi", "list_all")
pub fn list_all(table: EntityTable) -> List(Entity)

/// Check if an entity exists
@external(erlang, "auditor_entity_store_ffi", "exists")
pub fn exists(table: EntityTable, key: String) -> Bool

/// Count entities in the store
@external(erlang, "auditor_entity_store_ffi", "count")
pub fn count(table: EntityTable) -> Int

