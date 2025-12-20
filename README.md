# Chronicle - Audit Logging System

A demonstration of Enterprise Integration Patterns implemented in Gleam, featuring **Message Channels**, **Competing Consumers**, **Datatype Channels**, and **Dead Letter Queues** — all wrapped in a **Messaging Gateway** for transport abstraction.

## Quick Start

```bash
# Prerequisites: Gleam 1.0+, Docker, just

# 1. Start infrastructure (PostgreSQL + RabbitMQ)
just setup

# 2. Run the full application (producer + consumers)
just run

# 3. Send test events
just post                    # Send a basic event
just ingest                  # Send a random event
just flood 10                # Send 10 events in parallel
```

The server runs at `http://localhost:8080`.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                              HTTP API (Producer)                             │
│                            POST /events (JSON)                               │
└────────────────────────────────────┬────────────────────────────────────────┘
                                     │
                                     ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                         Topic Exchange (audit_events)                        │
│                                                                              │
│   Routes messages by event_type:                                             │
│     security.* → chronicle.security                                          │
│     user.*     → chronicle.users                                             │
│     billing.*  → chronicle.billing                                           │
│     #          → chronicle.all (catch-all)                                   │
└───────┬───────────────────┬───────────────────┬───────────────────┬─────────┘
        │                   │                   │                   │
        ▼                   ▼                   ▼                   ▼
┌───────────────┐   ┌───────────────┐   ┌───────────────┐   ┌───────────────┐
│   Security    │   │    Users      │   │   Billing     │   │   Analytics   │
│   Consumers   │   │   Consumers   │   │   Consumers   │   │   Consumers   │
└───────┬───────┘   └───────┬───────┘   └───────┬───────┘   └───────┬───────┘
        │                   │                   │                   │
        └───────────────────┴───────────────────┴───────────────────┘
                                     │
                                     ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                         PostgreSQL (Shared Database)                         │
│                            audit_events table                                │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Enterprise Integration Patterns

Chronicle demonstrates several patterns from *Enterprise Integration Patterns* by Hohpe & Woolf:

| Pattern | Implementation |
|---------|----------------|
| **Messaging Gateway** | `gateway.gleam` - abstracts OTP vs RabbitMQ |
| **Point-to-Point Channel** | Each queue delivers to exactly one consumer |
| **Competing Consumers** | Multiple consumers race for messages |
| **Datatype Channel** | Topic exchange routes by `event_type` |
| **Invalid Message Channel** | Dead letter queues for failed messages |
| **Pipes and Filters** | Event enrichment pipeline |

## Configuration

Chronicle follows [12-factor app](https://12factor.net/config) principles. All configuration via environment variables:

| Variable | Default | Description |
|----------|---------|-------------|
| `CHRONICLE_TRANSPORT` | `otp` | `otp` or `rabbitmq` |
| `CHRONICLE_MODE` | `full` | `full`, `producer`, or `consumer` |
| `CHRONICLE_STORE` | `postgres` | `postgres` or `ets` |
| `CHRONICLE_CONSUMER_ROLE` | `default` | Consumer role from `chronicle.toml` |
| `CHRONICLE_CONSUMER_COUNT` | `3` | Number of consumers per instance |
| `CHRONICLE_PORT` | `8080` | HTTP server port |

### Routing Configuration

Message routing is defined in `priv/chronicle.toml`:

```toml
[exchanges.audit_events]
type = "topic"
durable = true

[[routes]]
name = "security_events"
exchange = "audit_events"
queue = "chronicle.security"
routing_key = "security.#"
dead_letter_exchange = "dead_letter"
dead_letter_queue = "chronicle.security.dlq"

[[routes]]
name = "all_events"
exchange = "audit_events"
queue = "chronicle.all"
routing_key = "#"  # Catch-all

[[consumers]]
name = "security"
routes = ["security_events"]
instances = 2

[[consumers]]
name = "analytics"
routes = ["all_events"]
instances = 1
```

## Running Modes

### Full Mode (Default)
Runs both producer (HTTP API) and consumers:

```bash
just run
```

### Producer Only
Just the HTTP API, no consumers:

```bash
just producer
```

### Consumer Only
Process messages without HTTP server:

```bash
just consumer                           # Single consumer
just consumers 3                        # 3 consumers
just role=security consumers 2          # 2 security consumers
just transport=rabbitmq role=analytics consumers 1
```

### Composable Commands

Variables can be combined freely:

```bash
# Local development with ETS
just store=ets run

# RabbitMQ with specific role
just transport=rabbitmq role=security consumers 3

# Producer with RabbitMQ, consumers handle security events
# Terminal 1:
just transport=rabbitmq producer

# Terminal 2:
just transport=rabbitmq role=security consumers 2
```

## API Reference

### Create Event

```bash
curl -X POST http://localhost:8080/events \
  -H "Content-Type: application/json" \
  -d '{
    "actor": "alice@example.com",
    "action": "login",
    "resource_type": "security",
    "resource_id": "session-123"
  }'
```

**Response (202 Accepted):**
```json
{
  "status": "accepted",
  "id": "550e8400-e29b-41d4-a716-446655440000"
}
```

### Create Event with Explicit Type

```bash
curl -X POST http://localhost:8080/events \
  -H "Content-Type: application/json" \
  -d '{
    "actor": "alice@example.com",
    "action": "login",
    "resource_type": "auth",
    "resource_id": "session-123",
    "event_type": "security.login"
  }'
```

### List Events

```bash
curl http://localhost:8080/events | jq
```

### Health Check

```bash
curl http://localhost:8080/health
```

### Register Entity (for Enrichment)

```bash
curl -X POST http://localhost:8080/entities \
  -H "Content-Type: application/json" \
  -d '{
    "key": "org:acme",
    "name": "Acme Corp",
    "metadata": {"tier": "enterprise", "region": "us-west"}
  }'
```

## Testing

### Automated Tests

```bash
just test
```

### Manual Testing

```bash
# Basic event
just post

# Random event (different data each time)
just ingest

# Flood test (parallel events)
just flood 20

# Slow flood (see round-robin distribution)
just flood-slow 10
```

### Testing Datatype Channels

```bash
# Start RabbitMQ consumers for different roles
# Terminal 1: Security events only
just transport=rabbitmq role=security consumers 2

# Terminal 2: All events (analytics)
just transport=rabbitmq role=analytics consumers 1

# Terminal 3: Producer
just transport=rabbitmq producer

# Terminal 4: Send typed events
just send-security-event    # Goes to security + analytics
just send-user-event        # Goes to analytics only
just send-billing-event     # Goes to analytics only
```

### Inspect RabbitMQ

```bash
just list-queues            # Show all queues with message counts
just list-exchanges         # Show all exchanges
just rabbit-status          # Detailed queue status
```

## Just Commands Reference

```bash
just                        # Show all commands
just info                   # Show current configuration

# Build & Test
just build                  # Build the project
just test                   # Run tests
just check                  # Format + build + test

# Run
just run                    # Full mode (producer + consumer)
just producer               # Producer only
just consumer               # Single consumer
just consumers 3            # Multiple consumers

# Composable variables
just transport=rabbitmq run
just role=security consumers 2
just store=ets run

# Infrastructure
just setup                  # Start services + run migrations
just services-up            # Start PostgreSQL + RabbitMQ
just services-down          # Stop services

# Database
just migrate                # Run all migrations
just migrate-status         # Show migration status
just pg-shell               # PostgreSQL CLI
just pg-events              # Show recent events
just pg-count               # Count events

# RabbitMQ
just rabbit-up              # Start RabbitMQ
just rabbit-status          # Queue status
just list-queues            # All queues
just list-exchanges         # All exchanges

# API Testing
just health                 # Health check
just post                   # Create event
just events                 # List events
just ingest                 # Random event
just flood 20               # Parallel events

# Datatype Channel Testing
just send-security-event    # security.login event
just send-user-event        # user.created event
just send-billing-event     # billing.charge event
just show-routes            # Show configured routes
just show-consumers         # Show consumer roles

# Debug
just nodes                  # List Erlang nodes
just kill                   # Stop all Chronicle processes
```

## Project Structure

```
chronicle/
├── src/
│   ├── auditor.gleam              # Main entry point
│   └── auditor/
│       ├── config.gleam           # Environment configuration
│       ├── event.gleam            # Audit event type
│       ├── event_store.gleam      # Storage abstraction (ETS/PostgreSQL)
│       ├── gateway.gleam          # Messaging Gateway pattern
│       ├── channel.gleam          # OTP Point-to-Point Channel
│       ├── rabbit.gleam           # RabbitMQ backend
│       ├── routing_config.gleam   # TOML configuration parser
│       ├── topology.gleam         # RabbitMQ topology setup
│       ├── invalid_message.gleam  # Dead letter queue handling
│       ├── consumer.gleam         # Message processing logic
│       ├── filters.gleam          # Pipes and Filters
│       ├── entity_store.gleam     # Entity registry for enrichment
│       ├── router.gleam           # HTTP router (Wisp)
│       └── log.gleam              # Logging utilities
├── priv/
│   ├── chronicle.toml             # Routing configuration
│   └── migrations/                # Database migrations
├── test/                          # Test files
├── docs/                          # Additional documentation
├── docker-compose.yml             # PostgreSQL + RabbitMQ
├── gleam.toml                     # Dependencies
└── justfile                       # Task runner
```

## Dependencies

| Package | Purpose |
|---------|---------|
| `gleam_otp` | Actor-based concurrency |
| `carotte` | RabbitMQ client |
| `pog` | PostgreSQL client |
| `wisp` | Web framework |
| `mist` | HTTP server |
| `tom` | TOML configuration parsing |
| `cigogne` | Database migrations |
| `birl` | Date/time handling |
| `youid` | UUID generation |

## Part of Advent of Enterprise Integration Patterns

This project is part of the [Advent of Enterprise Integration Patterns](https://jamescarr.github.io) series, exploring messaging patterns while building Chronicle in Gleam.

- [Day 1: Point-to-Point Channel & Competing Consumers](/posts/2025-12-15-advent-of-eip-day-1/)
- [Day 2: Messaging Gateway & Pipes and Filters](/posts/2025-12-17-advent-of-eip-day-2/)
- [Day 3: Message Channels (Datatype, Pub/Sub, DLQ)](/posts/2025-12-20-advent-of-eip-day-3/)
