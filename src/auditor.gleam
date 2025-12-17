//// Chronicle - Audit Logging System
////
//// A demonstration of Enterprise Integration Patterns in Gleam,
//// specifically the Point-to-Point Channel pattern with Competing Consumers,
//// wrapped in a Messaging Gateway for transport abstraction.
////
//// The Messaging Gateway hides all transport complexity:
//// - Client code doesn't know if we're using OTP or RabbitMQ
//// - Consumer lifecycle is managed through the gateway
//// - Same code works for both local and distributed deployments

import auditor/config
import auditor/gateway
import auditor/log
import auditor/router.{Context}
import auditor/store
import gleam/erlang/process
import gleam/int
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
  log.info("Mode: " <> config.mode_name(cfg.mode))

  // Start the messaging gateway - abstracts OTP vs RabbitMQ
  let assert Ok(gateway_result) = gateway.start(cfg)
  log.info("Gateway started: " <> gateway_result.transport_name)

  // Initialize storage (only needed if we're consuming)
  let store = store.init()

  // Start consumers if this endpoint is configured as a consumer
  // The gateway handles all transport-specific details!
  let consumer_pool = case config.is_consumer(cfg) {
    False -> {
      log.info("Producer mode: not starting consumers")
      Error(Nil)
    }
    True -> {
      case
        gateway.start_consumers(
          gateway_result.gateway,
          cfg.consumer_count,
          store,
        )
      {
        Ok(pool) -> {
          log.info(
            "Started "
            <> int.to_string(gateway.consumer_count(pool))
            <> " consumers",
          )
          Ok(pool)
        }
        Error(msg) -> {
          log.error("Failed to start consumers: " <> msg)
          Error(Nil)
        }
      }
    }
  }

  // Build context for request handlers
  let ctx =
    Context(
      gateway: gateway_result.gateway,
      store: store,
      consumer_pool: consumer_pool,
    )

  // Start HTTP server if this endpoint is configured as a producer
  case config.is_producer(cfg) {
    True -> {
      let secret_key_base = wisp.random_string(64)
      let handler = fn(req) { router.handle_request(req, ctx) }

      let assert Ok(_) =
        handler
        |> wisp_mist.handler(secret_key_base)
        |> mist.new
        |> mist.port(cfg.port)
        |> mist.start

      log.info(
        "Chronicle running at http://localhost:" <> int.to_string(cfg.port),
      )
      log.info("  POST /events  - Create audit event")
      log.info("  GET  /events  - List all events")
      log.info("  GET  /health  - Health check")
    }
    False -> log.info("Consumer mode: HTTP server not started")
  }

  process.sleep_forever()
}
