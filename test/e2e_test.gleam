//// End-to-end tests for Chronicle API
////
//// These tests start a real server and make HTTP requests to verify behavior.

import auditor/channel
import auditor/consumer
import auditor/router.{Context}
import auditor/store
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/json
import gleam/list
import gleam/string
import mist
import wisp
import wisp/wisp_mist

const test_port = 9999

const base_url = "http://localhost:9999"

/// Start the server for testing
fn start_test_server() -> Nil {
  let table = store.init()
  let assert Ok(channel_started) = channel.start()
  let assert Ok(consumer_started) = consumer.start(channel_started.data, table)

  let ctx =
    Context(
      channel: channel_started.data,
      store: table,
      consumer: consumer_started.data,
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
fn get(path: String) -> Result(#(Int, String), httpc.HttpError) {
  let assert Ok(req) = request.to(base_url <> path)
  let req = request.set_header(req, "accept", "application/json")

  case httpc.send(req) {
    Ok(response) -> Ok(#(response.status, response.body))
    Error(e) -> Error(e)
  }
}

/// Helper to make POST requests with JSON body
fn post(path: String, body: String) -> Result(#(Int, String), httpc.HttpError) {
  let assert Ok(req) = request.to(base_url <> path)
  let req =
    req
    |> request.set_method(http.Post)
    |> request.set_header("content-type", "application/json")
    |> request.set_body(body)

  case httpc.send(req) {
    Ok(response) -> Ok(#(response.status, response.body))
    Error(e) -> Error(e)
  }
}

// =============================================================================
// Single E2E test that runs all scenarios
// =============================================================================

pub fn e2e_full_api_test() {
  // Start server once
  start_test_server()
  process.sleep(100)

  // Test 1: Health check
  let assert Ok(#(status, body)) = get("/health")
  let assert 200 = status
  let assert True = string.contains(body, "healthy")

  // Test 2: Create an event and verify it's returned
  let event_json =
    json.to_string(
      json.object([
        #("actor", json.string("test@example.com")),
        #("action", json.string("create")),
        #("resource_type", json.string("document")),
        #("resource_id", json.string("doc-123")),
      ]),
    )

  let assert Ok(#(create_status, create_body)) = post("/events", event_json)
  let assert 202 = create_status

  // Parse the response to get the ID
  let id_decoder = {
    use id <- decode.field("id", decode.string)
    decode.success(id)
  }
  let assert Ok(event_id) = json.parse(create_body, id_decoder)

  // Small delay for async processing
  process.sleep(50)

  // Test 3: List events and verify our event is there
  let assert Ok(#(list_status, list_body)) = get("/events")
  let assert 200 = list_status
  let assert True = string.contains(list_body, event_id)
  let assert True = string.contains(list_body, "test@example.com")
  let assert True = string.contains(list_body, "create")
  let assert True = string.contains(list_body, "document")
  let assert True = string.contains(list_body, "doc-123")

  // Test 4: Create multiple events
  let events = [
    #("alice@example.com", "read", "file", "file-1"),
    #("bob@example.com", "update", "file", "file-1"),
  ]

  list.each(events, fn(event) {
    let #(actor, action, resource_type, resource_id) = event
    let json_body =
      json.to_string(
        json.object([
          #("actor", json.string(actor)),
          #("action", json.string(action)),
          #("resource_type", json.string(resource_type)),
          #("resource_id", json.string(resource_id)),
        ]),
      )

    let assert Ok(#(s, _)) = post("/events", json_body)
    let assert 202 = s
  })

  process.sleep(100)

  // Verify all events are listed
  let assert Ok(#(_, all_body)) = get("/events")
  let assert True = string.contains(all_body, "alice@example.com")
  let assert True = string.contains(all_body, "bob@example.com")

  // Test 5: Invalid JSON returns 400
  let invalid_json = json.to_string(json.object([#("actor", json.string("x"))]))
  let assert Ok(#(bad_status, _)) = post("/events", invalid_json)
  let assert 400 = bad_status

  // Test 6: Not found returns 404
  let assert Ok(#(not_found_status, _)) = get("/nonexistent")
  let assert 404 = not_found_status

  Nil
}
