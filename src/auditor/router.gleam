//// HTTP Router - handles incoming audit event and entity requests
////
//// Provides a REST API for:
//// - Audit events: creating and listing (with pipeline processing)
//// - Entities: CRUD for enrichment lookup data
////
//// Events are processed through a Pipes and Filters pipeline before
//// being sent through the Messaging Gateway for async delivery.

import auditor/entity
import auditor/entity_store.{type EntityTable}
import auditor/event
import auditor/event_store.{type EventStore}
import auditor/filters
import auditor/gateway.{type ConsumerPool, type Gateway}
import auditor/log
import birl
import gleam/dict
import gleam/dynamic/decode
import gleam/http.{Delete, Get, Post}
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import wisp.{type Request as WispRequest, type Response as WispResponse}
import youid/uuid

/// Context passed to request handlers
pub type Context {
  Context(
    gateway: Gateway,
    store: EventStore,
    entity_store: EntityTable,
    consumer_pool: Result(ConsumerPool, Nil),
  )
}

/// Main router function
pub fn handle_request(req: WispRequest, ctx: Context) -> WispResponse {
  use <- wisp.log_request(req)
  use <- wisp.rescue_crashes

  case wisp.path_segments(req) {
    ["events"] -> handle_events(req, ctx)
    ["entities"] -> handle_entities(req, ctx)
    ["entities", key] -> handle_entity(req, ctx, key)
    ["health"] -> health_check(ctx)
    _ -> wisp.not_found()
  }
}

// =============================================================================
// Events API
// =============================================================================

/// Handle /events endpoint
fn handle_events(req: WispRequest, ctx: Context) -> WispResponse {
  case req.method {
    Post -> create_event(req, ctx)
    Get -> list_events(ctx)
    _ -> wisp.method_not_allowed([Post, Get])
  }
}

/// POST /events - create a new audit event
/// Supports optional enrichment fields: entity_key, correlation_id
fn create_event(req: WispRequest, ctx: Context) -> WispResponse {
  use body <- wisp.require_string_body(req)

  // Decode required and optional fields
  let decoder = {
    use actor <- decode.field("actor", decode.string)
    use action <- decode.field("action", decode.string)
    use resource_type <- decode.field("resource_type", decode.string)
    use resource_id <- decode.field("resource_id", decode.string)
    use entity_key <- decode.optional_field(
      "entity_key",
      None,
      decode.string |> decode.map(Some),
    )
    use correlation_id <- decode.optional_field(
      "correlation_id",
      None,
      decode.string |> decode.map(Some),
    )
    decode.success(#(
      actor,
      action,
      resource_type,
      resource_id,
      entity_key,
      correlation_id,
    ))
  }

  case json.parse(body, decoder) {
    Ok(#(actor, action, resource_type, resource_id, entity_key, correlation_id)) -> {
      let id = uuid.v4_string()
      let timestamp = birl.utc_now() |> birl.to_iso8601

      // Create event with optional enrichment fields
      let raw_event =
        event.new_with_enrichment(
          id,
          actor,
          action,
          resource_type,
          resource_id,
          timestamp,
          correlation_id,
          entity_key,
        )

      // Process through the ingestion pipeline (validate/normalize, no enrichment)
      // Enrichment happens at read time, not write time
      case filters.ingest(raw_event) {
        Ok(processed_event) -> {
          // Send through the gateway
          gateway.send_event(ctx.gateway, processed_event)

          // Notify consumers
          case ctx.consumer_pool {
            Ok(pool) -> gateway.notify_consumers(pool)
            Error(_) -> Nil
          }

          log.info("Queued event " <> id)

          wisp.response(202)
          |> wisp.json_body(
            json.to_string(
              json.object([
                #("status", json.string("accepted")),
                #("id", json.string(id)),
              ]),
            ),
          )
        }
        Error(reason) -> {
          log.warn("Event rejected by pipeline: " <> reason)

          wisp.response(422)
          |> wisp.json_body(
            json.to_string(
              json.object([
                #("error", json.string("validation_failed")),
                #("reason", json.string(reason)),
              ]),
            ),
          )
        }
      }
    }
    Error(_) -> {
      wisp.bad_request("Invalid JSON format")
      |> wisp.json_body(
        json.to_string(json.object([#("error", json.string("Invalid JSON"))])),
      )
    }
  }
}

/// GET /events - list all events, hydrated with entity data
fn list_events(ctx: Context) -> WispResponse {
  let events = event_store.list_all(ctx.store)

  // Hydrate events with current entity data at read time
  let hydrated_events =
    events
    |> list.map(fn(evt) { hydrate_event(evt, ctx.entity_store) })

  let events_json =
    hydrated_events
    |> list.map(event.to_json)
    |> json.preprocessed_array

  wisp.ok()
  |> wisp.json_body(json.to_string(events_json))
}

/// Hydrate an event with entity data at read time
/// This ensures events always reflect current entity state
fn hydrate_event(evt: event.AuditEvent, entity_table: EntityTable) -> event.AuditEvent {
  case evt.entity_key {
    Some(key) -> {
      case entity_store.get(entity_table, key) {
        Ok(ent) -> {
          // Merge entity attributes into event metadata
          let enriched_metadata =
            evt.metadata
            |> dict.insert("entity_name", ent.name)
            |> dict.merge(ent.attributes)
          event.AuditEvent(..evt, metadata: enriched_metadata)
        }
        Error(Nil) -> evt  // Entity not found, return as-is
      }
    }
    None -> evt
  }
}

// =============================================================================
// Entities API
// =============================================================================

/// Handle /entities endpoint (collection)
fn handle_entities(req: WispRequest, ctx: Context) -> WispResponse {
  case req.method {
    Post -> create_entity(req, ctx)
    Get -> list_entities(ctx)
    _ -> wisp.method_not_allowed([Post, Get])
  }
}

/// Handle /entities/:key endpoint (single entity)
fn handle_entity(req: WispRequest, ctx: Context, key: String) -> WispResponse {
  case req.method {
    Get -> get_entity(ctx, key)
    Delete -> delete_entity(ctx, key)
    _ -> wisp.method_not_allowed([Get, Delete])
  }
}

/// POST /entities - create a new entity
fn create_entity(req: WispRequest, ctx: Context) -> WispResponse {
  use body <- wisp.require_string_body(req)

  // Decode entity JSON
  let decoder = {
    use key <- decode.field("key", decode.string)
    use name <- decode.field("name", decode.string)
    use attrs <- decode.optional_field(
      "attributes",
      dict.new(),
      decode.dict(decode.string, decode.string),
    )
    decode.success(#(key, name, attrs))
  }

  case json.parse(body, decoder) {
    Ok(#(key, name, attrs)) -> {
      let ent = entity.new_with_attributes(key, name, attrs)
      entity_store.put(ctx.entity_store, ent)

      log.info("Created entity: " <> key)

      wisp.response(201)
      |> wisp.json_body(
        json.to_string(
          json.object([
            #("status", json.string("created")),
            #("key", json.string(key)),
          ]),
        ),
      )
    }
    Error(_) -> {
      wisp.bad_request("Invalid JSON format")
      |> wisp.json_body(
        json.to_string(
          json.object([
            #("error", json.string("Invalid JSON. Required: key, name")),
          ]),
        ),
      )
    }
  }
}

/// GET /entities - list all entities
fn list_entities(ctx: Context) -> WispResponse {
  let entities = entity_store.list_all(ctx.entity_store)

  let entities_json =
    entities
    |> list.map(entity.to_json)
    |> json.preprocessed_array

  wisp.ok()
  |> wisp.json_body(json.to_string(entities_json))
}

/// GET /entities/:key - get a single entity
fn get_entity(ctx: Context, key: String) -> WispResponse {
  case entity_store.get(ctx.entity_store, key) {
    Ok(ent) -> {
      wisp.ok()
      |> wisp.json_body(json.to_string(entity.to_json(ent)))
    }
    Error(Nil) -> {
      wisp.response(404)
      |> wisp.json_body(
        json.to_string(
          json.object([#("error", json.string("Entity not found: " <> key))]),
        ),
      )
    }
  }
}

/// DELETE /entities/:key - delete an entity
fn delete_entity(ctx: Context, key: String) -> WispResponse {
  case entity_store.exists(ctx.entity_store, key) {
    True -> {
      entity_store.delete(ctx.entity_store, key)
      log.info("Deleted entity: " <> key)

      wisp.ok()
      |> wisp.json_body(
        json.to_string(
          json.object([
            #("status", json.string("deleted")),
            #("key", json.string(key)),
          ]),
        ),
      )
    }
    False -> {
      wisp.response(404)
      |> wisp.json_body(
        json.to_string(
          json.object([#("error", json.string("Entity not found: " <> key))]),
        ),
      )
    }
  }
}

// =============================================================================
// Health Check
// =============================================================================

/// Health check endpoint
fn health_check(ctx: Context) -> WispResponse {
  let transport = case gateway.is_distributed(ctx.gateway) {
    True -> "rabbitmq"
    False -> "otp"
  }

  let entity_count = entity_store.count(ctx.entity_store)

  wisp.ok()
  |> wisp.json_body(
    json.to_string(
      json.object([
        #("status", json.string("healthy")),
        #("transport", json.string(transport)),
        #("entities", json.int(entity_count)),
      ]),
    ),
  )
}
