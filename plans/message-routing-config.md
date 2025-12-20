# Message Routing Configuration Plan

## Overview

Implement configuration-file-based message routing for Chronicle, enabling:
- Declarative route definitions (event types → channels)
- Automatic RabbitMQ topology creation (exchanges, queues, bindings)
- Named consumer roles that can be started independently
- Support for Datatype Channels and Publish-Subscribe patterns

## Goals

1. **Declarative Configuration**: Define routes in a TOML config file, not environment variables
2. **Automatic Topology**: Chronicle creates required RabbitMQ exchanges/queues on startup
3. **Named Consumers**: Define consumer roles that handle specific event types
4. **Flexible Execution**: Run all consumers, specific consumers, or producer-only mode

## Configuration File Format

Location: `priv/chronicle.toml`

```toml
# Chronicle Message Routing Configuration

[rabbitmq]
host = "localhost"
port = 5672
user = "guest"
password = "guest"
vhost = "/"

# Main exchange for all audit events (fanout for pub-sub)
[exchanges.audit_events]
type = "topic"
durable = true

# Dead letter exchange for failed messages
[exchanges.dead_letter]
type = "fanout"
durable = true

# ============================================================================
# Route Definitions
# ============================================================================
# Routes define how events flow from the main exchange to specific queues
# based on routing key patterns (event type matching)

[[routes]]
name = "security_events"
description = "Security-related audit events"
exchange = "audit_events"
queue = "chronicle.security"
routing_key = "security.*"  # Matches security.login, security.logout, etc.
dead_letter_exchange = "dead_letter"
dead_letter_queue = "chronicle.security.dlq"

[[routes]]
name = "user_events"
description = "User activity events"
exchange = "audit_events"
queue = "chronicle.users"
routing_key = "user.*"
dead_letter_exchange = "dead_letter"
dead_letter_queue = "chronicle.users.dlq"

[[routes]]
name = "billing_events"
description = "Billing and payment events"
exchange = "audit_events"
queue = "chronicle.billing"
routing_key = "billing.*"
dead_letter_exchange = "dead_letter"
dead_letter_queue = "chronicle.billing.dlq"

[[routes]]
name = "all_events"
description = "Catch-all for analytics/archival"
exchange = "audit_events"
queue = "chronicle.all"
routing_key = "#"  # Matches everything

# ============================================================================
# Consumer Definitions
# ============================================================================
# Consumers are named roles that process messages from specific routes

[[consumers]]
name = "security"
description = "Security team consumer"
routes = ["security_events"]
instances = 2  # Number of competing consumers

[[consumers]]
name = "analytics"
description = "Analytics pipeline consumer"
routes = ["all_events"]
instances = 1

[[consumers]]
name = "billing"
description = "Billing system consumer"
routes = ["billing_events"]
instances = 1

[[consumers]]
name = "default"
description = "Full consumer (all routes)"
routes = ["security_events", "user_events", "billing_events", "all_events"]
instances = 3
```

## Event Type Mapping

Events need a "type" field that maps to routing keys:

```gleam
// Event with explicit type for routing
pub type AuditEvent {
  AuditEvent(
    id: String,
    event_type: String,        // NEW: e.g., "security.login", "user.created"
    actor: String,
    action: String,
    resource_type: String,
    resource_id: String,
    timestamp: String,
    correlation_id: Option(String),
    entity_key: Option(String),
    metadata: Dict(String, String),
  )
}
```

**Routing key derivation:**
- Explicit: Use `event_type` field if present
- Implicit: Derive from `resource_type.action` (e.g., `user.create`)

## Implementation Steps

### Phase 1: Configuration Parsing

**Files to create/modify:**
- `src/auditor/routing_config.gleam` - Config types and TOML parsing
- `priv/chronicle.toml` - Default configuration file

**Types:**

```gleam
pub type RoutingConfig {
  RoutingConfig(
    rabbitmq: RabbitConfig,
    exchanges: List(ExchangeConfig),
    routes: List(RouteConfig),
    consumers: List(ConsumerConfig),
  )
}

pub type ExchangeConfig {
  ExchangeConfig(
    name: String,
    exchange_type: ExchangeType,  // topic, fanout, direct
    durable: Bool,
  )
}

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

pub type ConsumerConfig {
  ConsumerConfig(
    name: String,
    description: String,
    routes: List(String),  // Route names
    instances: Int,
  )
}
```

### Phase 2: RabbitMQ Topology Management

**Files to create/modify:**
- `src/auditor/topology.gleam` - Exchange/queue/binding creation

**Responsibilities:**
1. Connect to RabbitMQ
2. Declare all configured exchanges
3. Declare all configured queues (with DLQ settings)
4. Create bindings between exchanges and queues
5. Idempotent: safe to run multiple times

```gleam
/// Ensure all configured topology exists in RabbitMQ
pub fn ensure_topology(config: RoutingConfig) -> Result(Nil, String)

/// Declare an exchange
pub fn declare_exchange(conn, config: ExchangeConfig) -> Result(Nil, String)

/// Declare a queue with optional dead-letter settings
pub fn declare_queue(conn, route: RouteConfig) -> Result(Nil, String)

/// Bind queue to exchange with routing key
pub fn bind_queue(conn, route: RouteConfig) -> Result(Nil, String)
```

### Phase 3: Updated Gateway

**Files to modify:**
- `src/auditor/gateway.gleam` - Use routing keys when publishing

**Changes:**
1. `send_event` derives routing key from event type
2. Publishes to configured exchange with routing key
3. Consumer startup uses route configuration

```gleam
/// Publish with routing key derived from event type
pub fn send_event(gateway: Gateway, event: AuditEvent) -> Nil {
  let routing_key = derive_routing_key(event)
  // Publish to exchange with routing_key
}

/// Start consumers for a specific named consumer config
pub fn start_named_consumer(
  gateway: Gateway,
  consumer_name: String,
  config: RoutingConfig,
  store: EventStore,
) -> Result(ConsumerPool, String)
```

### Phase 4: CLI Integration

**Files to modify:**
- `src/auditor/config.gleam` - Add consumer role selection
- `src/auditor.gleam` - Handle consumer role parameter
- `justfile` - Add commands for specific consumers

**New environment variable:**
```bash
CHRONICLE_CONSUMER=security  # Run only the "security" consumer
CHRONICLE_CONSUMER=all       # Run all consumers (default)
```

**New just commands:**
```bash
just run                      # Full mode with all consumers
just run consumer=security    # Run only security consumer
just run consumer=billing     # Run only billing consumer
just producer                 # Producer only (no consumers)
just consumers                # All consumers, no HTTP server
```

### Phase 5: Dead Letter Queue Handling

**Files to create:**
- `src/auditor/dead_letter.gleam` - DLQ consumer and inspection

**Capabilities:**
1. Messages that fail processing N times go to DLQ
2. DLQ messages include original routing key and failure reason
3. Optional: DLQ consumer for alerting/monitoring

## File Structure After Implementation

```
chronicle/
├── priv/
│   ├── chronicle.toml          # NEW: Routing configuration
│   ├── cigogne.toml
│   └── migrations/
├── src/
│   └── auditor/
│       ├── routing_config.gleam # NEW: Config parsing
│       ├── topology.gleam       # NEW: RabbitMQ topology
│       ├── dead_letter.gleam    # NEW: DLQ handling
│       ├── gateway.gleam        # MODIFIED: Routing key support
│       ├── config.gleam         # MODIFIED: Consumer role selection
│       └── ...
└── justfile                     # MODIFIED: New commands
```

## Example Usage

### Starting Different Consumer Roles

```bash
# Start everything (producer + all consumers)
just run

# Start only the security consumer (2 instances per config)
just run consumer=security

# Start only the billing consumer
just run consumer=billing

# Start producer only (HTTP API, publishes to exchange)
just producer

# Start all consumers, no HTTP server
just consumers
```

### Publishing Events with Types

```bash
# Security event - routed to security queue
curl -X POST localhost:8080/events \
  -H "Content-Type: application/json" \
  -d '{
    "event_type": "security.login",
    "actor": "alice@acme.com",
    "action": "login",
    "resource_type": "session",
    "resource_id": "sess-123"
  }'

# User event - routed to users queue
curl -X POST localhost:8080/events \
  -H "Content-Type: application/json" \
  -d '{
    "event_type": "user.created",
    "actor": "admin@acme.com",
    "action": "create",
    "resource_type": "user",
    "resource_id": "user-456"
  }'
```

### Inspecting Topology

```bash
# View RabbitMQ management UI
open http://localhost:15672

# List queues via API
curl -u guest:guest http://localhost:15672/api/queues | jq '.[].name'

# View bindings
curl -u guest:guest http://localhost:15672/api/bindings | jq
```

## Testing Strategy

1. **Unit tests**: Config parsing, routing key derivation
2. **Integration tests**: Topology creation, message routing
3. **E2E tests**: Full flow with multiple consumer types

## Open Questions

1. **Config file location**: `priv/chronicle.toml` or configurable via env var?
2. **Hot reload**: Should config changes require restart or support hot reload?
3. **Event type validation**: Reject events with unknown types or let them through?
4. **OTP fallback**: Should OTP mode also respect routing config (with in-process routing)?

## Dependencies

May need to add:
- `tom` - TOML parsing (already have via cigogne)
- Possibly update `carotte` for more exchange/binding control

## Milestones

1. [ ] Phase 1: Configuration parsing and types
2. [ ] Phase 2: RabbitMQ topology management
3. [ ] Phase 3: Updated gateway with routing
4. [ ] Phase 4: CLI integration and just commands
5. [ ] Phase 5: Dead letter queue handling
6. [ ] Tests and documentation

