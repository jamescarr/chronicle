# RabbitMQ Setup for Chronicle

This guide explains how to run Chronicle with RabbitMQ as the message transport, including Datatype Channels for routing events by type.

## Prerequisites

- Docker and Docker Compose
- Chronicle dependencies installed (`gleam deps download`)
- just (recommended for convenience)

## Quick Start

```bash
# 1. Start all services (PostgreSQL + RabbitMQ)
just setup

# 2. Run with RabbitMQ transport
just transport=rabbitmq run
```

This starts:
- **PostgreSQL**: `localhost:5432`
- **RabbitMQ AMQP**: `localhost:5672`
- **RabbitMQ Management UI**: [http://localhost:15672](http://localhost:15672) (guest/guest)

## Configuration

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `CHRONICLE_TRANSPORT` | `otp` | `otp` or `rabbitmq` |
| `CHRONICLE_MODE` | `full` | `full`, `producer`, or `consumer` |
| `CHRONICLE_CONSUMER_ROLE` | `default` | Consumer role from `chronicle.toml` |
| `RABBITMQ_HOST` | `localhost` | RabbitMQ hostname |
| `RABBITMQ_PORT` | `5672` | AMQP port |
| `RABBITMQ_USER` | `guest` | Username |
| `RABBITMQ_PASS` | `guest` | Password |
| `RABBITMQ_VHOST` | `/` | Virtual host |

### Routing Configuration

Edit `priv/chronicle.toml` to customize routing:

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
name = "user_events"
exchange = "audit_events"
queue = "chronicle.users"
routing_key = "user.#"

[[consumers]]
name = "security"
routes = ["security_events"]
instances = 2

[[consumers]]
name = "analytics"
routes = ["all_events"]
instances = 1
```

## Architecture

### Simple Mode (Single Queue)

```
┌─────────────────┐
│   HTTP API      │
│  POST /events   │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  RabbitMQ       │
│  chronicle.events
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│   Consumers     │
│   (competing)   │
└─────────────────┘
```

### Datatype Channel Mode (Topic Routing)

```
┌──────────────────────────────────────────────────────────────────┐
│                        HTTP API                                   │
│  POST /events { "event_type": "security.login", ... }            │
└────────────────────────────┬─────────────────────────────────────┘
                             │
                             ▼
┌──────────────────────────────────────────────────────────────────┐
│                  Topic Exchange (audit_events)                    │
│                                                                   │
│   Routing Key     │  Queue                                        │
│   ─────────────────────────────────────────                       │
│   security.#      │  chronicle.security                           │
│   user.#          │  chronicle.users                              │
│   billing.#       │  chronicle.billing                            │
│   #               │  chronicle.all (catch-all)                    │
└───────┬───────────────────┬───────────────────┬───────────────────┘
        │                   │                   │
        ▼                   ▼                   ▼
┌───────────────┐   ┌───────────────┐   ┌───────────────┐
│   Security    │   │    Users      │   │   Analytics   │
│   Consumers   │   │   Consumers   │   │   Consumers   │
│  (role-based) │   │  (role-based) │   │  (all events) │
└───────────────┘   └───────────────┘   └───────────────┘
```

## Running Separate Producers and Consumers

### Production Pattern

In production, run producers and consumers as separate processes:

**Terminal 1: Producer (HTTP API)**
```bash
just transport=rabbitmq producer
```

**Terminal 2: Security Consumers**
```bash
just transport=rabbitmq role=security consumers 2
```

**Terminal 3: Analytics Consumer**
```bash
just transport=rabbitmq role=analytics consumers 1
```

### Testing Routing

```bash
# Send different event types
just send-security-event    # Routes to security + analytics
just send-user-event        # Routes to users + analytics
just send-billing-event     # Routes to billing + analytics

# Check queue status
just list-queues
```

### Example Output

```bash
$ just list-queues
RabbitMQ queues:
{
  "name": "chronicle.security",
  "messages": 0,
  "consumers": 2
}
{
  "name": "chronicle.users",
  "messages": 0,
  "consumers": 0
}
{
  "name": "chronicle.all",
  "messages": 0,
  "consumers": 1
}
```

## Scaling Consumers

Scale specific consumer roles independently:

```bash
# Scale security team during incident
just transport=rabbitmq role=security consumers 5

# Scale analytics for batch processing
just transport=rabbitmq role=analytics consumers 3
```

Each consumer instance gets unique messages from its subscribed queues (competing consumers pattern).

## Dead Letter Queues

Failed messages automatically route to dead letter queues:

```
chronicle.security → (on failure) → chronicle.security.dlq
chronicle.users    → (on failure) → chronicle.users.dlq
chronicle.billing  → (on failure) → chronicle.billing.dlq
```

Check DLQ status:
```bash
curl -s -u guest:guest http://localhost:15672/api/queues/%2F | \
  jq '.[] | select(.name | endswith(".dlq")) | {name, messages}'
```

## Monitoring

### Management UI

Open [http://localhost:15672](http://localhost:15672) to see:
- Queue depths and rates
- Consumer connections
- Exchange bindings
- Message publishing rates

### Just Commands

```bash
just rabbit-status      # Queue status with consumer count
just list-queues        # All queues with message counts
just list-exchanges     # All exchanges
just rabbit-consumers   # Consumer details
```

## Comparison: OTP vs RabbitMQ

| Feature | OTP Mode | RabbitMQ Mode |
|---------|----------|---------------|
| Setup | None | Requires RabbitMQ |
| Distribution | Single process | Multi-machine |
| Durability | In-memory | Persistent |
| Routing | Single channel | Topic exchange |
| Dead letters | None | Automatic DLQ |
| Scaling | Limited | Horizontal |
| Use case | Development | Production |

## Troubleshooting

### Connection Refused

```bash
# Check if RabbitMQ is running
docker-compose ps

# Check logs
docker-compose logs rabbitmq
```

### Messages Not Routing

1. Check the event has an `event_type` field
2. Verify routing key matches patterns in `chronicle.toml`
3. Ensure queues are bound to the exchange:
   ```bash
   just list-exchanges
   ```

### Consumers Not Receiving

1. Verify consumers are subscribed:
   ```bash
   just rabbit-consumers
   ```
2. Check the consumer role matches routes in `chronicle.toml`
3. Ensure `CHRONICLE_MODE=consumer` is set

### DLQ Messages Piling Up

Inspect failed messages:
```bash
# View DLQ contents
docker-compose exec rabbitmq rabbitmqctl list_queues name messages | grep dlq
```

## Cleanup

```bash
# Stop services
just services-down

# Remove all data (fresh start)
docker-compose down -v
just setup
```
