//// RabbitMQ Topology Management
////
//// Creates exchanges, queues, and bindings based on routing configuration.
//// Ensures the RabbitMQ topology exists before consumers start.
////
//// This module implements the Datatype Channel pattern by creating
//// separate queues for different event types, bound via routing keys.

import auditor/log
import auditor/routing_config.{
  type ExchangeConfig, type RouteConfig, type RoutingConfig, Direct, Fanout,
  Topic,
}
import carotte
import carotte/channel.{type Channel}
import carotte/exchange
import carotte/queue
import gleam/dict
import gleam/list
import gleam/option.{None, Some}
import gleam/result

// =============================================================================
// Topology Setup
// =============================================================================

/// Ensure all configured topology exists in RabbitMQ
/// This is idempotent - safe to call multiple times
pub fn ensure_topology(
  ch: Channel,
  config: RoutingConfig,
) -> Result(Nil, String) {
  log.info("Setting up RabbitMQ topology...")

  // 1. Declare all exchanges
  use _ <- result.try(declare_exchanges(ch, config))

  // 2. Declare all queues (including DLQs)
  use _ <- result.try(declare_queues(ch, config))

  // 3. Create bindings
  use _ <- result.try(create_bindings(ch, config))

  log.info("RabbitMQ topology setup complete")
  Ok(Nil)
}

// =============================================================================
// Exchange Management
// =============================================================================

/// Declare all configured exchanges
fn declare_exchanges(ch: Channel, config: RoutingConfig) -> Result(Nil, String) {
  config.exchanges
  |> dict.values
  |> list.try_each(fn(ex_config) { declare_exchange(ch, ex_config) })
}

/// Declare a single exchange
pub fn declare_exchange(ch: Channel, config: ExchangeConfig) -> Result(Nil, String) {
  let ex_type = case config.exchange_type {
    Topic -> exchange.Topic
    Fanout -> exchange.Fanout
    Direct -> exchange.Direct
  }

  let ex =
    exchange.new(config.name)
    |> exchange.with_type(ex_type)
    |> apply_durable(config.durable)

  case exchange.declare(ex, ch) {
    Ok(_) -> {
      log.info("Declared exchange: " <> config.name)
      Ok(Nil)
    }
    Error(e) -> Error("Failed to declare exchange " <> config.name <> ": " <> carotte_error_to_string(e))
  }
}

fn apply_durable(ex: exchange.Exchange, durable: Bool) -> exchange.Exchange {
  case durable {
    True -> exchange.as_durable(ex)
    False -> ex
  }
}

// =============================================================================
// Queue Management
// =============================================================================

/// Declare all configured queues (main queues and DLQs)
fn declare_queues(ch: Channel, config: RoutingConfig) -> Result(Nil, String) {
  config.routes
  |> list.try_each(fn(route) { declare_route_queues(ch, route) })
}

/// Declare the main queue and optional DLQ for a route
fn declare_route_queues(ch: Channel, route: RouteConfig) -> Result(Nil, String) {
  // First, declare the DLQ if configured
  use _ <- result.try(case route.dead_letter_queue {
    Some(dlq_name) -> declare_simple_queue(ch, dlq_name)
    None -> Ok(Nil)
  })

  // Then declare the main queue with DLX settings
  declare_queue_with_dlx(ch, route)
}

/// Declare a simple durable queue (for DLQs)
fn declare_simple_queue(ch: Channel, name: String) -> Result(Nil, String) {
  let q =
    queue.new(name)
    |> queue.as_durable

  case queue.declare(q, ch) {
    Ok(declared) -> {
      log.info("Declared queue: " <> declared.name)
      Ok(Nil)
    }
    Error(e) -> Error("Failed to declare queue " <> name <> ": " <> carotte_error_to_string(e))
  }
}

/// Declare a queue with dead letter exchange settings
fn declare_queue_with_dlx(ch: Channel, route: RouteConfig) -> Result(Nil, String) {
  case route.dead_letter_exchange {
    Some(dlx) -> {
      // Use FFI for queue with arguments
      case declare_queue_with_args(ch, route.queue, dlx) {
        Ok(_) -> {
          log.info("Declared queue with DLX: " <> route.queue <> " -> " <> dlx)
          Ok(Nil)
        }
        Error(e) -> Error("Failed to declare queue " <> route.queue <> ": " <> e)
      }
    }
    None -> declare_simple_queue(ch, route.queue)
  }
}

/// Declare a queue with dead letter exchange argument (via FFI)
@external(erlang, "auditor_topology_ffi", "declare_queue_with_dlx")
fn declare_queue_with_args(
  ch: Channel,
  queue_name: String,
  dead_letter_exchange: String,
) -> Result(String, String)

// =============================================================================
// Binding Management
// =============================================================================

/// Create all queue-to-exchange bindings
fn create_bindings(ch: Channel, config: RoutingConfig) -> Result(Nil, String) {
  config.routes
  |> list.try_each(fn(route) { create_route_bindings(ch, route) })
}

/// Create bindings for a single route
fn create_route_bindings(ch: Channel, route: RouteConfig) -> Result(Nil, String) {
  // Bind main queue to exchange
  use _ <- result.try(bind_queue(ch, route.queue, route.exchange, route.routing_key))

  // Bind DLQ to dead letter exchange if configured
  case route.dead_letter_exchange, route.dead_letter_queue {
    Some(dlx), Some(dlq) -> bind_queue(ch, dlq, dlx, "#")
    _, _ -> Ok(Nil)
  }
}

/// Bind a queue to an exchange with a routing key
fn bind_queue(
  ch: Channel,
  queue_name: String,
  exchange_name: String,
  routing_key: String,
) -> Result(Nil, String) {
  case queue.bind(
    channel: ch,
    queue: queue_name,
    exchange: exchange_name,
    routing_key: routing_key,
  ) {
    Ok(_) -> {
      log.info(
        "Bound queue " <> queue_name <> " to " <> exchange_name <> " with key: " <> routing_key,
      )
      Ok(Nil)
    }
    Error(e) -> Error(
      "Failed to bind " <> queue_name <> " to " <> exchange_name <> ": " <> carotte_error_to_string(e),
    )
  }
}

// =============================================================================
// Helpers
// =============================================================================

fn carotte_error_to_string(error: carotte.CarotteError) -> String {
  case error {
    carotte.Blocked -> "Connection blocked"
    carotte.Closed -> "Connection closed"
    carotte.AuthFailure(msg) -> "Auth failure: " <> msg
    carotte.ProcessNotFound -> "Process not found"
    carotte.AlreadyRegistered(name) -> "Already registered: " <> name
    carotte.NotFound(msg) -> "Not found: " <> msg
    carotte.AccessRefused(msg) -> "Access refused: " <> msg
    carotte.PreconditionFailed(msg) -> "Precondition failed: " <> msg
    carotte.ResourceLocked(msg) -> "Resource locked: " <> msg
    carotte.ChannelClosed(msg) -> "Channel closed: " <> msg
    carotte.ConnectionRefused(msg) -> "Connection refused: " <> msg
    carotte.ConnectionTimeout(msg) -> "Connection timeout: " <> msg
    carotte.FrameError(msg) -> "Frame error: " <> msg
    carotte.InternalError(msg) -> "Internal error: " <> msg
    carotte.InvalidPath(msg) -> "Invalid path: " <> msg
    carotte.NoRoute(msg) -> "No route: " <> msg
    carotte.NotAllowed(msg) -> "Not allowed: " <> msg
    carotte.NotImplemented(msg) -> "Not implemented: " <> msg
    carotte.UnexpectedFrame(msg) -> "Unexpected frame: " <> msg
    carotte.CommandInvalid(msg) -> "Command invalid: " <> msg
    carotte.UnknownError(msg) -> "Unknown error: " <> msg
  }
}

