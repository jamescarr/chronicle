//// Chronicle - Audit Logging System
////
//// A demonstration of Enterprise Integration Patterns in Gleam,
//// specifically the Point-to-Point Channel pattern with Competing Consumers,
//// wrapped in a Messaging Gateway for transport abstraction.

import auditor/config
import auditor/consumer
import auditor/gateway
import auditor/log
import auditor/router.{Context}
import auditor/store
import gleam/erlang/process
import gleam/int
import gleam/list
import logging
import mist
import wisp
import wisp/wisp_mist

pub fn main() -> Nil {
  logging.configure()
  logging.set_level(logging.Info)

  log.info("Starting Chronicle...")

  // Load configuration from environment
  let cfg = config.load()
  log.info("Transport: " <> config.transport_name(cfg.transport))
  log.info("Consumer count: " <> int.to_string(cfg.consumer_count))

  // Initialize storage
  let table = store.init()

  // Start the messaging gateway
  let assert Ok(gateway_result) = gateway.start(cfg)
  log.info("Gateway started: " <> gateway_result.transport_name)

  // Start consumers based on transport type
  let consumers = case gateway.is_distributed(gateway_result.gateway) {
    False -> {
      // OTP mode: start local competing consumers
      case gateway.get_otp_channel(gateway_result.gateway) {
        Ok(channel) -> {
          let consumers = consumer.start_pool(cfg.consumer_count, channel, table)
          log.info(
            "Started "
            <> int.to_string(list.length(consumers))
            <> " competing consumers",
          )
          consumers
        }
        Error(_) -> []
      }
    }
    True -> {
      // RabbitMQ mode: consumers will connect independently
      log.info("RabbitMQ mode: consumers connect to queue independently")
      []
    }
  }

  // Build context for request handlers
  let ctx =
    Context(gateway: gateway_result.gateway, store: table, consumers: consumers)

  // Start HTTP server
  let secret_key_base = wisp.random_string(64)
  let handler = fn(req) { router.handle_request(req, ctx) }

  let assert Ok(_) =
    handler
    |> wisp_mist.handler(secret_key_base)
    |> mist.new
    |> mist.port(cfg.port)
    |> mist.start

  log.info("Chronicle running at http://localhost:" <> int.to_string(cfg.port))
  log.info("  POST /events  - Create audit event")
  log.info("  GET  /events  - List all events")
  log.info("  GET  /health  - Health check")

  process.sleep_forever()
}
