//// HTTP Router - handles incoming audit event requests
////
//// Provides a REST API for creating and listing audit events.
//// Events are sent through a Messaging Gateway for async processing.
////
//// The router demonstrates the Gateway pattern - it doesn't know
//// whether events go to OTP actors or RabbitMQ. It just calls
//// gateway.send_event() and the abstraction handles the rest.

import auditor/event
import auditor/gateway.{type ConsumerPool, type Gateway}
import auditor/log
import auditor/store.{type Table}
import birl
import gleam/dynamic/decode
import gleam/http.{Get, Post}
import gleam/json
import gleam/list
import wisp.{type Request as WispRequest, type Response as WispResponse}
import youid/uuid

/// Context passed to request handlers
pub type Context {
  Context(
    gateway: Gateway,
    store: Table,
    consumer_pool: Result(ConsumerPool, Nil),
  )
}

/// Main router function
pub fn handle_request(req: WispRequest, ctx: Context) -> WispResponse {
  use <- wisp.log_request(req)
  use <- wisp.rescue_crashes

  case wisp.path_segments(req) {
    ["events"] -> handle_events(req, ctx)
    ["health"] -> health_check(ctx)
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

  let decoder = {
    use actor <- decode.field("actor", decode.string)
    use action <- decode.field("action", decode.string)
    use resource_type <- decode.field("resource_type", decode.string)
    use resource_id <- decode.field("resource_id", decode.string)
    decode.success(#(actor, action, resource_type, resource_id))
  }

  case json.parse(body, decoder) {
    Ok(#(actor, action, resource_type, resource_id)) -> {
      let id = uuid.v4_string()
      let timestamp = birl.utc_now() |> birl.to_iso8601

      let audit_event =
        event.new(id, actor, action, resource_type, resource_id, timestamp)

      // Send through the gateway - doesn't matter if it's OTP or RabbitMQ!
      gateway.send_event(ctx.gateway, audit_event)

      // Notify consumers - gateway handles the transport-specific details
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
    Error(_) -> {
      wisp.bad_request("Invalid JSON format")
      |> wisp.json_body(
        json.to_string(json.object([#("error", json.string("Invalid JSON"))])),
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
fn health_check(ctx: Context) -> WispResponse {
  let transport = case gateway.is_distributed(ctx.gateway) {
    True -> "rabbitmq"
    False -> "otp"
  }

  wisp.ok()
  |> wisp.json_body(
    json.to_string(
      json.object([
        #("status", json.string("healthy")),
        #("transport", json.string(transport)),
      ]),
    ),
  )
}
