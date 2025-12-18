//// Entity - enrichment data for the Pipes and Filters pattern
////
//// Entities are registered reference data that can be used to enrich
//// audit events. When an event includes an entity_key, the enrichment
//// filter looks up the entity and merges its attributes into the event's
//// metadata.
////
//// Example use cases:
//// - "org:acme" -> Organization metadata (tier, region, owner)
//// - "user:123" -> User metadata (department, role)
//// - "project:alpha" -> Project metadata (status, team)

import gleam/dict.{type Dict}
import gleam/json.{type Json}
import gleam/list

/// An entity for enrichment lookups
pub type Entity {
  Entity(
    key: String,
    name: String,
    attributes: Dict(String, String),
  )
}

/// Create a new entity
pub fn new(key: String, name: String) -> Entity {
  Entity(key:, name:, attributes: dict.new())
}

/// Create an entity with attributes
pub fn new_with_attributes(
  key: String,
  name: String,
  attributes: Dict(String, String),
) -> Entity {
  Entity(key:, name:, attributes:)
}

/// Add an attribute to an entity
pub fn with_attribute(entity: Entity, key: String, value: String) -> Entity {
  Entity(..entity, attributes: dict.insert(entity.attributes, key, value))
}

/// Encode an entity to JSON
pub fn to_json(entity: Entity) -> Json {
  let attrs_json =
    entity.attributes
    |> dict.to_list
    |> list.map(fn(pair) { #(pair.0, json.string(pair.1)) })
    |> json.object

  json.object([
    #("key", json.string(entity.key)),
    #("name", json.string(entity.name)),
    #("attributes", attrs_json),
  ])
}

