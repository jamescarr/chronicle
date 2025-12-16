//// Chronicle - Audit Logging System
////
//// A demonstration of Enterprise Integration Patterns in Gleam,
//// specifically the Point-to-Point Channel pattern.

import auditor/channel
import auditor/consumer
import auditor/router.{Context}
import auditor/store
import gleam/erlang/process
import logging
import mist
import wisp
import wisp/wisp_mist

pub fn main() -> Nil {
  logging.configure()
  logging.set_level(logging.Info)

  logging.log(logging.Info, "Starting Chronicle...")

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
    |> mist.port(8080)
    |> mist.start

  logging.log(logging.Info, "Chronicle running at http://localhost:8080")
  logging.log(logging.Info, "  POST /events  - Create audit event")
  logging.log(logging.Info, "  GET  /events  - List all events")
  logging.log(logging.Info, "  GET  /health  - Health check")

  process.sleep_forever()
}
