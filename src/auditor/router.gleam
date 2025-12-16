/// HTTP Router - handles incoming requests
import auditor/channel.{type ChannelMessage}
import auditor/consumer
import auditor/event
import auditor/store.{type Table}
import gleam/dynamic/decode
import gleam/erlang/process.{type Subject}
import gleam/http.{Get, Post}
import gleam/io
import gleam/json
import gleam/list
import wisp.{type Request as WispRequest, type Response as WispResponse}

/// Context passed to request handlers
pub type Context {
  Context(
    channel: Subject(ChannelMessage),
    store: Table,
    consumer: Subject(consumer.ConsumerMessage),
  )
}

/// Main router function
pub fn handle_request(req: WispRequest, ctx: Context) -> WispResponse {
  use <- wisp.log_request(req)
  use <- wisp.rescue_crashes

  case wisp.path_segments(req) {
    ["events"] -> handle_events(req, ctx)
    ["health"] -> health_check()
    _ -> wisp.not_found()
  }
}

/// Handle /events endpoint
fn handle_events(req: WispRequest, ctx: Context) -> WispResponse {
  case req.method {
    Post -> create_event(req, ctx)
    Get -> list_events(ctx)
    _ -> wisp.method_not_allowed([Post, Get])
  }
}

/// POST /events - create a new audit event
fn create_event(req: WispRequest, ctx: Context) -> WispResponse {
  use body <- wisp.require_string_body(req)

  // Define the decoder for incoming event JSON
  let decoder = {
    use actor <- decode.field("actor", decode.string)
    use action <- decode.field("action", decode.string)
    use resource_type <- decode.field("resource_type", decode.string)
    use resource_id <- decode.field("resource_id", decode.string)
    decode.success(#(actor, action, resource_type, resource_id))
  }

  case json.parse(body, decoder) {
    Ok(#(actor, action, resource_type, resource_id)) -> {
      // Generate ID and timestamp
      let id = generate_id()
      let timestamp = get_timestamp()

      let audit_event =
        event.new(id, actor, action, resource_type, resource_id, timestamp)

      // Send to channel (async!)
      channel.send(ctx.channel, audit_event)

      // Trigger consumer to process
      consumer.poll(ctx.consumer)

      io.println("API: Queued event " <> id)

      // Return 202 Accepted - event is queued, not yet processed
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
    Error(_) -> {
      wisp.bad_request("Invalid JSON format")
      |> wisp.json_body(
        json.to_string(
          json.object([#("error", json.string("Invalid JSON"))]),
        ),
      )
    }
  }
}

/// GET /events - list all events
fn list_events(ctx: Context) -> WispResponse {
  let events = store.list_all(ctx.store)

  let events_json =
    events
    |> list.map(event.to_json)
    |> json.preprocessed_array

  wisp.ok()
  |> wisp.json_body(json.to_string(events_json))
}

/// Health check endpoint
fn health_check() -> WispResponse {
  wisp.ok()
  |> wisp.json_body(
    json.to_string(json.object([#("status", json.string("healthy"))])),
  )
}

/// Generate a simple UUID-ish ID
@external(erlang, "auditor_router_ffi", "generate_id")
fn generate_id() -> String

/// Get current timestamp as ISO string
@external(erlang, "auditor_router_ffi", "get_timestamp")
fn get_timestamp() -> String
