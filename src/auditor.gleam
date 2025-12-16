/// Chronicle - Audit Logging System
/// Day 1: Point-to-Point Channel with ETS Storage
import auditor/channel
import auditor/consumer
import auditor/router.{Context}
import auditor/store
import gleam/erlang/process
import gleam/io
import mist
import wisp
import wisp/wisp_mist

pub fn main() -> Nil {
  // Initialize ETS store
  io.println("Initializing ETS store...")
  let table = store.init()

  // Start the Point-to-Point Channel (OTP actor)
  io.println("Starting message channel...")
  let assert Ok(channel_started) = channel.start()
  let channel_subject = channel_started.data

  // Start the Consumer (reads from channel, writes to store)
  io.println("Starting consumer...")
  let assert Ok(consumer_started) = consumer.start(channel_subject, table)
  let consumer_subject = consumer_started.data

  // Create the request context
  let ctx =
    Context(channel: channel_subject, store: table, consumer: consumer_subject)

  // Configure Wisp
  let secret_key_base = wisp.random_string(64)

  // Create the handler
  let handler = fn(req) { router.handle_request(req, ctx) }

  // Start the HTTP server
  io.println("Starting HTTP server on port 8080...")
  let assert Ok(_) =
    handler
    |> wisp_mist.handler(secret_key_base)
    |> mist.new
    |> mist.port(8080)
    |> mist.start

  io.println("Chronicle is running! http://localhost:8080")
  io.println("")
  io.println("Endpoints:")
  io.println("  POST /events  - Create an audit event")
  io.println("  GET  /events  - List all events")
  io.println("  GET  /health  - Health check")
  io.println("")

  // Keep the process alive
  process.sleep_forever()
}
