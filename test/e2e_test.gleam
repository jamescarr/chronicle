//// End-to-end tests for Chronicle API
////
//// These tests start a real server and make HTTP requests to verify behavior.

import auditor/config
import auditor/gateway
import auditor/router.{Context}
import auditor/store
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/list
import gleam/result
import gleam/string
import gleeunit/should
import mist
import wisp
import wisp/wisp_mist

const test_port = 9999

const base_url = "http://localhost:9999"

/// Start the server for testing
fn start_test_server() -> Nil {
  let table = store.init()

  // Use default OTP transport for tests
  let cfg = config.load()
  let assert Ok(gateway_result) = gateway.start(cfg)

  // Start consumers through the gateway - clean abstraction!
  let consumer_pool =
    gateway.start_consumers(gateway_result.gateway, 2, table)
    |> result.replace_error(Nil)

  let ctx =
    Context(
      gateway: gateway_result.gateway,
      store: table,
      consumer_pool: consumer_pool,
    )

  let secret_key_base = wisp.random_string(64)
  let handler = fn(req) { router.handle_request(req, ctx) }

  let assert Ok(_) =
    handler
    |> wisp_mist.handler(secret_key_base)
    |> mist.new
    |> mist.port(test_port)
    |> mist.start

  Nil
}

/// Helper to make GET requests
fn get(path: String) -> #(Int, String) {
  let assert Ok(req) = request.to(base_url <> path)
  let req = request.set_header(req, "accept", "application/json")

  httpc.send(req)
  |> should.be_ok
  |> fn(resp) { #(resp.status, resp.body) }
}

/// Helper to make POST requests with JSON body
fn post(path: String, body: String) -> #(Int, String) {
  let assert Ok(req) = request.to(base_url <> path)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body)

  httpc.send(req)
  |> should.be_ok
  |> fn(resp) { #(resp.status, resp.body) }
}

/// Build event JSON from components
fn event_json(
  actor: String,
  action: String,
  resource_type: String,
  resource_id: String,
) -> String {
  json.to_string(
    json.object([
      #("actor", json.string(actor)),
      #("action", json.string(action)),
      #("resource_type", json.string(resource_type)),
      #("resource_id", json.string(resource_id)),
    ]),
  )
}

// =============================================================================
// E2E Test
// =============================================================================

pub fn e2e_full_api_test() {
  // Start server once
  start_test_server()
  process.sleep(100)

  // Test 1: Health check
  let #(status, body) = get("/health")
  status |> should.equal(200)
  body |> string.contains("healthy") |> should.be_true

  // Test 2: Create an event
  let #(create_status, create_body) =
    post(
      "/events",
      event_json("test@example.com", "create", "document", "doc-123"),
    )

  create_status |> should.equal(202)
  create_body |> string.contains("accepted") |> should.be_true

  // Parse the event ID from response
  let id_decoder = decode.field("id", decode.string, decode.success)
  let event_id =
    json.parse(create_body, id_decoder)
    |> should.be_ok

  // Small delay for async processing
  process.sleep(50)

  // Test 3: List events and verify our event is there
  let #(list_status, list_body) = get("/events")
  list_status |> should.equal(200)
  list_body |> string.contains(event_id) |> should.be_true
  list_body |> string.contains("test@example.com") |> should.be_true
  list_body |> string.contains("create") |> should.be_true
  list_body |> string.contains("document") |> should.be_true
  list_body |> string.contains("doc-123") |> should.be_true

  // Test 4: Create multiple events
  [
    #("alice@example.com", "read", "file", "file-1"),
    #("bob@example.com", "update", "file", "file-1"),
  ]
  |> list.each(fn(e) {
    let #(actor, action, resource_type, resource_id) = e
    let #(s, _) =
      post("/events", event_json(actor, action, resource_type, resource_id))
    s |> should.equal(202)
  })

  process.sleep(100)

  // Verify all events are listed
  let #(_, all_body) = get("/events")
  all_body |> string.contains("alice@example.com") |> should.be_true
  all_body |> string.contains("bob@example.com") |> should.be_true

  // Test 5: Invalid JSON returns 400
  let invalid_json = json.to_string(json.object([#("actor", json.string("x"))]))
  let #(bad_status, _) = post("/events", invalid_json)
  bad_status |> should.equal(400)

  // Test 6: Not found returns 404
  let #(not_found_status, _) = get("/nonexistent")
  not_found_status |> should.equal(404)
}
