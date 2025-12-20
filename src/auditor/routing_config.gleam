//// Routing Configuration
////
//// Parses the chronicle.toml configuration file to define:
//// - RabbitMQ connection settings
//// - Exchange definitions
//// - Route definitions (routing key → queue mappings)
//// - Consumer role definitions
////
//// This enables Datatype Channels via topic exchanges and routing keys.

import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None}
import gleam/result
import gleam/string
import tom

// =============================================================================
// Configuration Types
// =============================================================================

/// Complete routing configuration
pub type RoutingConfig {
  RoutingConfig(
    rabbitmq: RabbitMqConfig,
    exchanges: Dict(String, ExchangeConfig),
    routes: List(RouteConfig),
    consumers: List(ConsumerConfig),
  )
}

/// RabbitMQ connection configuration
pub type RabbitMqConfig {
  RabbitMqConfig(
    host: String,
    port: Int,
    user: String,
    password: String,
    vhost: String,
  )
}

/// Exchange type
pub type ExchangeType {
  Topic
  Fanout
  Direct
}

/// Exchange configuration
pub type ExchangeConfig {
  ExchangeConfig(
    name: String,
    exchange_type: ExchangeType,
    durable: Bool,
    description: String,
  )
}

/// Route configuration - maps routing keys to queues
pub type RouteConfig {
  RouteConfig(
    name: String,
    description: String,
    exchange: String,
    queue: String,
    routing_key: String,
    dead_letter_exchange: Option(String),
    dead_letter_queue: Option(String),
  )
}

/// Consumer role configuration
pub type ConsumerConfig {
  ConsumerConfig(
    name: String,
    description: String,
    routes: List(String),
    instances: Int,
  )
}

// =============================================================================
// Default Configuration
// =============================================================================

/// Default RabbitMQ configuration
pub fn default_rabbitmq() -> RabbitMqConfig {
  RabbitMqConfig(
    host: "localhost",
    port: 5672,
    user: "guest",
    password: "guest",
    vhost: "/",
  )
}

/// Default routing configuration (single queue, no routing)
pub fn default_config() -> RoutingConfig {
  RoutingConfig(
    rabbitmq: default_rabbitmq(),
    exchanges: dict.from_list([
      #(
        "audit_events",
        ExchangeConfig(
          name: "audit_events",
          exchange_type: Topic,
          durable: True,
          description: "Main audit events exchange",
        ),
      ),
    ]),
    routes: [
      RouteConfig(
        name: "all_events",
        description: "Default catch-all route",
        exchange: "audit_events",
        queue: "chronicle.events",
        routing_key: "#",
        dead_letter_exchange: None,
        dead_letter_queue: None,
      ),
    ],
    consumers: [
      ConsumerConfig(
        name: "default",
        description: "Default consumer",
        routes: ["all_events"],
        instances: 3,
      ),
    ],
  )
}

// =============================================================================
// Configuration Loading
// =============================================================================

/// Load configuration from the priv/chronicle.toml file
pub fn load() -> Result(RoutingConfig, String) {
  case read_config_file() {
    Ok(content) -> parse_config(content)
    Error(err) -> Error("Failed to read config file: " <> err)
  }
}

/// Load configuration, falling back to defaults on error
pub fn load_or_default() -> RoutingConfig {
  case load() {
    Ok(config) -> config
    Error(_) -> default_config()
  }
}

/// Read the configuration file from priv directory
fn read_config_file() -> Result(String, String) {
  case read_priv_file("chronicle.toml") {
    Ok(content) -> Ok(content)
    Error(_) -> Error("chronicle.toml not found in priv directory")
  }
}

@external(erlang, "auditor_routing_config_ffi", "read_priv_file")
fn read_priv_file(filename: String) -> Result(String, String)

// =============================================================================
// TOML Parsing
// =============================================================================

/// Parse TOML content into RoutingConfig
pub fn parse_config(content: String) -> Result(RoutingConfig, String) {
  case tom.parse(content) {
    Ok(toml) -> {
      let rabbitmq = parse_rabbitmq(toml)
      let exchanges = parse_exchanges(toml)
      let routes = parse_routes(toml)
      let consumers = parse_consumers(toml)

      Ok(RoutingConfig(
        rabbitmq: rabbitmq,
        exchanges: exchanges,
        routes: routes,
        consumers: consumers,
      ))
    }
    Error(err) -> Error("TOML parse error: " <> tom_error_to_string(err))
  }
}

fn tom_error_to_string(err: tom.ParseError) -> String {
  case err {
    tom.Unexpected(got, expected) ->
      "Unexpected '" <> got <> "', expected " <> expected
    tom.KeyAlreadyInUse(keys) ->
      "Key already in use: " <> string.join(keys, ".")
  }
}

/// Parse [rabbitmq] section
fn parse_rabbitmq(toml: Dict(String, tom.Toml)) -> RabbitMqConfig {
  let default = default_rabbitmq()

  RabbitMqConfig(
    host: get_string(toml, ["rabbitmq", "host"])
      |> result.unwrap(default.host),
    port: get_int(toml, ["rabbitmq", "port"])
      |> result.unwrap(default.port),
    user: get_string(toml, ["rabbitmq", "user"])
      |> result.unwrap(default.user),
    password: get_string(toml, ["rabbitmq", "password"])
      |> result.unwrap(default.password),
    vhost: get_string(toml, ["rabbitmq", "vhost"])
      |> result.unwrap(default.vhost),
  )
}

/// Parse [exchanges.*] sections
fn parse_exchanges(toml: Dict(String, tom.Toml)) -> Dict(String, ExchangeConfig) {
  case tom.get_table(toml, ["exchanges"]) {
    Ok(exchanges_table) -> {
      exchanges_table
      |> dict.to_list
      |> list.filter_map(fn(pair) {
        let #(name, value) = pair
        case value {
          tom.InlineTable(table) | tom.Table(table) ->
            Ok(#(name, parse_exchange(name, table)))
          _ -> Error(Nil)
        }
      })
      |> dict.from_list
    }
    Error(_) -> dict.new()
  }
}

fn parse_exchange(name: String, table: Dict(String, tom.Toml)) -> ExchangeConfig {
  ExchangeConfig(
    name: name,
    exchange_type: get_string_from_table(table, "type")
      |> result.map(parse_exchange_type)
      |> result.unwrap(Topic),
    durable: get_bool_from_table(table, "durable")
      |> result.unwrap(True),
    description: get_string_from_table(table, "description")
      |> result.unwrap(""),
  )
}

fn parse_exchange_type(type_str: String) -> ExchangeType {
  case string.lowercase(type_str) {
    "topic" -> Topic
    "fanout" -> Fanout
    "direct" -> Direct
    _ -> Topic
  }
}

/// Parse [[routes]] array
fn parse_routes(toml: Dict(String, tom.Toml)) -> List(RouteConfig) {
  case tom.get_array(toml, ["routes"]) {
    Ok(routes_array) -> {
      routes_array
      |> list.filter_map(fn(item) {
        case item {
          tom.InlineTable(table) | tom.Table(table) -> Ok(parse_route(table))
          _ -> Error(Nil)
        }
      })
    }
    Error(_) -> []
  }
}

fn parse_route(table: Dict(String, tom.Toml)) -> RouteConfig {
  RouteConfig(
    name: get_string_from_table(table, "name")
      |> result.unwrap("unnamed"),
    description: get_string_from_table(table, "description")
      |> result.unwrap(""),
    exchange: get_string_from_table(table, "exchange")
      |> result.unwrap("audit_events"),
    queue: get_string_from_table(table, "queue")
      |> result.unwrap("chronicle.events"),
    routing_key: get_string_from_table(table, "routing_key")
      |> result.unwrap("#"),
    dead_letter_exchange: get_string_from_table(table, "dead_letter_exchange")
      |> option.from_result,
    dead_letter_queue: get_string_from_table(table, "dead_letter_queue")
      |> option.from_result,
  )
}

/// Parse [[consumers]] array
fn parse_consumers(toml: Dict(String, tom.Toml)) -> List(ConsumerConfig) {
  case tom.get_array(toml, ["consumers"]) {
    Ok(consumers_array) -> {
      consumers_array
      |> list.filter_map(fn(item) {
        case item {
          tom.InlineTable(table) | tom.Table(table) -> Ok(parse_consumer(table))
          _ -> Error(Nil)
        }
      })
    }
    Error(_) -> []
  }
}

fn parse_consumer(table: Dict(String, tom.Toml)) -> ConsumerConfig {
  ConsumerConfig(
    name: get_string_from_table(table, "name")
      |> result.unwrap("default"),
    description: get_string_from_table(table, "description")
      |> result.unwrap(""),
    routes: get_string_array_from_table(table, "routes")
      |> result.unwrap([]),
    instances: get_int_from_table(table, "instances")
      |> result.unwrap(1),
  )
}

// =============================================================================
// TOML Helpers
// =============================================================================

fn get_string(toml: Dict(String, tom.Toml), path: List(String)) -> Result(String, Nil) {
  tom.get_string(toml, path)
  |> result.replace_error(Nil)
}

fn get_int(toml: Dict(String, tom.Toml), path: List(String)) -> Result(Int, Nil) {
  tom.get_int(toml, path)
  |> result.replace_error(Nil)
}

fn get_string_from_table(table: Dict(String, tom.Toml), key: String) -> Result(String, Nil) {
  case dict.get(table, key) {
    Ok(tom.String(s)) -> Ok(s)
    _ -> Error(Nil)
  }
}

fn get_int_from_table(table: Dict(String, tom.Toml), key: String) -> Result(Int, Nil) {
  case dict.get(table, key) {
    Ok(tom.Int(i)) -> Ok(i)
    _ -> Error(Nil)
  }
}

fn get_bool_from_table(table: Dict(String, tom.Toml), key: String) -> Result(Bool, Nil) {
  case dict.get(table, key) {
    Ok(tom.Bool(b)) -> Ok(b)
    _ -> Error(Nil)
  }
}

fn get_string_array_from_table(
  table: Dict(String, tom.Toml),
  key: String,
) -> Result(List(String), Nil) {
  case dict.get(table, key) {
    Ok(tom.Array(arr)) -> {
      arr
      |> list.filter_map(fn(item) {
        case item {
          tom.String(s) -> Ok(s)
          _ -> Error(Nil)
        }
      })
      |> Ok
    }
    _ -> Error(Nil)
  }
}

// =============================================================================
// Configuration Queries
// =============================================================================

/// Get a consumer configuration by name
pub fn get_consumer(config: RoutingConfig, name: String) -> Option(ConsumerConfig) {
  config.consumers
  |> list.find(fn(c) { c.name == name })
  |> option.from_result
}

/// Get a route configuration by name
pub fn get_route(config: RoutingConfig, name: String) -> Option(RouteConfig) {
  config.routes
  |> list.find(fn(r) { r.name == name })
  |> option.from_result
}

/// Get all routes for a consumer
pub fn get_consumer_routes(
  config: RoutingConfig,
  consumer: ConsumerConfig,
) -> List(RouteConfig) {
  consumer.routes
  |> list.filter_map(fn(route_name) {
    get_route(config, route_name)
    |> option.to_result(Nil)
  })
}

/// Get all queue names from routes
pub fn get_all_queues(config: RoutingConfig) -> List(String) {
  config.routes
  |> list.map(fn(r) { r.queue })
  |> list.unique
}

/// Get the exchange for a route
pub fn get_exchange_for_route(
  config: RoutingConfig,
  route: RouteConfig,
) -> Option(ExchangeConfig) {
  dict.get(config.exchanges, route.exchange)
  |> option.from_result
}

