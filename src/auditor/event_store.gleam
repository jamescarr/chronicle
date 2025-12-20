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
import auditor/log
import birl
import gleam/dict
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{Some}
import gleam/time/timestamp
import pog

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
    insert: fn(evt) { ets_insert(table, evt) },
    list_all: fn() { ets_list_all(table) },
    get: fn(id) { ets_get(table, id) },
  )
}

@external(erlang, "auditor_store_ffi", "init")
fn ets_init() -> EtsTable

@external(erlang, "auditor_store_ffi", "insert")
fn ets_insert(table: EtsTable, evt: AuditEvent) -> Bool

@external(erlang, "auditor_store_ffi", "list_all")
fn ets_list_all(table: EtsTable) -> List(AuditEvent)

@external(erlang, "auditor_store_ffi", "get")
fn ets_get(table: EtsTable, id: String) -> Result(AuditEvent, Nil)

// =============================================================================
// PostgreSQL Backend
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
pub fn create_postgres(config: PostgresConfig) -> EventStore {
  // Create a unique pool name for this store instance
  let pool_name = process.new_name(prefix: "chronicle_pg_pool")

  // Configure the connection pool
  let db_config =
    pog.default_config(pool_name)
    |> pog.host(config.host)
    |> pog.port(config.port)
    |> pog.database(config.database)
    |> pog.user(config.user)
    |> pog.password(Some(config.password))
    |> pog.pool_size(10)

  // Start the connection pool
  case pog.start(db_config) {
    Ok(started) -> {
      let conn = started.data
      log.info("PostgreSQL connection pool started")

      EventStore(
        insert: fn(evt) { pg_insert(conn, evt) },
        list_all: fn() { pg_list_all(conn) },
        get: fn(id) { pg_get(conn, id) },
      )
    }
    Error(_) -> {
      log.error("Failed to start PostgreSQL pool, falling back to ETS")
      create_ets()
    }
  }
}

// =============================================================================
// PostgreSQL Operations
// =============================================================================

fn pg_insert(conn: pog.Connection, evt: AuditEvent) -> Bool {
  let metadata_json = metadata_to_json(evt.metadata)

  // Convert ISO8601 timestamp string to pog timestamp
  let ts = case birl.parse(evt.timestamp) {
    Ok(t) -> {
      let unix_micro = birl.to_unix_micro(t)
      let seconds = unix_micro / 1_000_000
      let nanoseconds = { unix_micro % 1_000_000 } * 1000
      timestamp.from_unix_seconds_and_nanoseconds(seconds, nanoseconds)
    }
    Error(_) -> timestamp.system_time()
  }

  let result =
    pog.query(
      "
      INSERT INTO audit_events (id, actor, action, resource_type, resource_id, timestamp, event_type, correlation_id, entity_key, metadata)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10::jsonb)
      ON CONFLICT (id) DO NOTHING
    ",
    )
    |> pog.parameter(pog.text(evt.id))
    |> pog.parameter(pog.text(evt.actor))
    |> pog.parameter(pog.text(evt.action))
    |> pog.parameter(pog.text(evt.resource_type))
    |> pog.parameter(pog.text(evt.resource_id))
    |> pog.parameter(pog.timestamp(ts))
    |> pog.parameter(pog.nullable(pog.text, evt.event_type))
    |> pog.parameter(pog.nullable(pog.text, evt.correlation_id))
    |> pog.parameter(pog.nullable(pog.text, evt.entity_key))
    |> pog.parameter(pog.text(metadata_json))
    |> pog.execute(conn)

  case result {
    Ok(_) -> True
    Error(err) -> {
      log.error("PostgreSQL insert failed: " <> pg_error_to_string(err))
      False
    }
  }
}

fn pg_list_all(conn: pog.Connection) -> List(AuditEvent) {
  let result =
    pog.query(
      "
      SELECT id, actor, action, resource_type, resource_id, 
             to_char(timestamp, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"') as timestamp,
             event_type, correlation_id, entity_key, metadata::text
      FROM audit_events
      ORDER BY timestamp DESC
    ",
    )
    |> pog.returning(event_row_decoder())
    |> pog.execute(conn)

  case result {
    Ok(returned) -> returned.rows
    Error(err) -> {
      log.error("PostgreSQL list_all failed: " <> pg_error_to_string(err))
      []
    }
  }
}

fn pg_get(conn: pog.Connection, id: String) -> Result(AuditEvent, Nil) {
  let result =
    pog.query(
      "
      SELECT id, actor, action, resource_type, resource_id,
             to_char(timestamp, 'YYYY-MM-DD\"T\"HH24:MI:SS\"Z\"') as timestamp,
             event_type, correlation_id, entity_key, metadata::text
      FROM audit_events
      WHERE id = $1
    ",
    )
    |> pog.parameter(pog.text(id))
    |> pog.returning(event_row_decoder())
    |> pog.execute(conn)

  case result {
    Ok(returned) -> {
      case returned.rows {
        [evt, ..] -> Ok(evt)
        [] -> Error(Nil)
      }
    }
    Error(err) -> {
      log.error("PostgreSQL get failed: " <> pg_error_to_string(err))
      Error(Nil)
    }
  }
}

// =============================================================================
// PostgreSQL Helpers
// =============================================================================

fn event_row_decoder() -> decode.Decoder(AuditEvent) {
  use id <- decode.field(0, decode.string)
  use actor <- decode.field(1, decode.string)
  use action <- decode.field(2, decode.string)
  use resource_type <- decode.field(3, decode.string)
  use resource_id <- decode.field(4, decode.string)
  use timestamp <- decode.field(5, decode.string)
  use event_type <- decode.field(6, decode.optional(decode.string))
  use correlation_id <- decode.field(7, decode.optional(decode.string))
  use entity_key <- decode.field(8, decode.optional(decode.string))
  use metadata_json <- decode.field(9, decode.string)

  let metadata = json_to_metadata(metadata_json)

  decode.success(event.AuditEvent(
    id: id,
    actor: actor,
    action: action,
    resource_type: resource_type,
    resource_id: resource_id,
    timestamp: timestamp,
    event_type: event_type,
    correlation_id: correlation_id,
    entity_key: entity_key,
    metadata: metadata,
  ))
}

fn metadata_to_json(metadata: dict.Dict(String, String)) -> String {
  case dict.is_empty(metadata) {
    True -> "{}"
    False -> {
      metadata
      |> dict.to_list
      |> list.map(fn(pair) { #(pair.0, json.string(pair.1)) })
      |> json.object
      |> json.to_string
    }
  }
}

fn json_to_metadata(json_str: String) -> dict.Dict(String, String) {
  let decoder = decode.dict(decode.string, decode.string)
  case json.parse(json_str, decoder) {
    Ok(d) -> d
    Error(_) -> dict.new()
  }
}

fn pg_error_to_string(err: pog.QueryError) -> String {
  case err {
    pog.ConstraintViolated(msg, constraint, _) ->
      "Constraint violated: " <> constraint <> " - " <> msg
    pog.PostgresqlError(code, name, msg) ->
      "PostgreSQL error [" <> code <> "/" <> name <> "]: " <> msg
    pog.UnexpectedArgumentCount(expected, got) ->
      "Unexpected argument count: expected "
      <> int_to_string(expected)
      <> ", got "
      <> int_to_string(got)
    pog.UnexpectedArgumentType(expected, got) ->
      "Unexpected argument type: expected " <> expected <> ", got " <> got
    pog.UnexpectedResultType(_) -> "Unexpected result type"
    pog.QueryTimeout -> "Query timeout"
    pog.ConnectionUnavailable -> "Connection unavailable"
  }
}

@external(erlang, "erlang", "integer_to_binary")
fn int_to_string(n: Int) -> String
