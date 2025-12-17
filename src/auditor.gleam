//// Chronicle - Audit Logging System
////
//// A demonstration of Enterprise Integration Patterns in Gleam,
//// specifically the Point-to-Point Channel pattern with Competing Consumers,
//// wrapped in a Messaging Gateway for transport abstraction.
////
//// Supports configurable Messaging Endpoint modes:
//// - Full: both produces and consumes (default)
//// - Producer: HTTP API only, publishes to channel
//// - Consumer: subscribes to channel, stores events

import auditor/config
import auditor/consumer
import auditor/gateway
import auditor/log
import auditor/router.{Context}
import auditor/store
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/option.{None, Some}
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

  // Initialize storage (consumers need it to store events)
  let table = case config.is_consumer(cfg) {
    True -> Some(store.init())
    False -> None
  }

  // Start the messaging gateway
  let assert Ok(gateway_result) = gateway.start(cfg)
  log.info("Gateway started: " <> gateway_result.transport_name)

  // Start consumers if this endpoint is configured as a consumer
  let consumers = case config.is_consumer(cfg) {
    False -> {
      log.info("Producer mode: not starting consumers")
      []
    }
    True -> {
      case gateway.is_distributed(gateway_result.gateway) {
        False -> {
          // OTP mode: start local competing consumers
          case gateway.get_otp_channel(gateway_result.gateway), table {
            Ok(channel), Some(st) -> {
              let consumers =
                consumer.start_pool(cfg.consumer_count, channel, st)
              log.info(
                "Started "
                <> int.to_string(list.length(consumers))
                <> " competing consumers",
              )
              consumers
            }
            _, _ -> []
          }
        }
        True -> {
          // RabbitMQ mode: subscribe to the queue
          case table {
            Some(st) -> {
              log.info("RabbitMQ consumer mode: subscribing to queue...")
              case
                gateway.subscribe_events(gateway_result.gateway, fn(event) {
                  log.info("Processing event: " <> event.id)
                  let _ = store.insert(st, event)
                  Nil
                })
              {
                Ok(tag) -> log.info("Subscribed with consumer tag: " <> tag)
                Error(msg) -> log.error("Failed to subscribe: " <> msg)
              }
              []
            }
            None -> []
          }
        }
      }
    }
  }

  // Build context for request handlers (needed for producer mode)
  let ctx =
    Context(
      gateway: gateway_result.gateway,
      store: case table {
        Some(st) -> st
        None -> store.init()
      },
      consumers: consumers,
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
    False -> {
      log.info("Consumer mode: HTTP server not started")
    }
  }

  process.sleep_forever()
}
