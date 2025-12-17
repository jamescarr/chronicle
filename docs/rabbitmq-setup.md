# RabbitMQ Setup for Chronicle

This guide explains how to run Chronicle with RabbitMQ as the message transport.

## Prerequisites

- Docker and Docker Compose
- Chronicle dependencies installed (`gleam deps download`)

## Quick Start

### 1. Start RabbitMQ

```bash
docker-compose up -d
```

This starts RabbitMQ with the management plugin. You can access:
- **AMQP**: `localhost:5672`
- **Management UI**: [http://localhost:15672](http://localhost:15672)
  - Username: `guest`
  - Password: `guest`

### 2. Configure Chronicle for RabbitMQ

Copy the environment template and enable RabbitMQ:

```bash
cp .env.example .env
```

Edit `.env` to set:

```bash
CHRONICLE_TRANSPORT=rabbitmq
```

### 3. Run Chronicle

Load the environment and start:

```bash
export $(cat .env | xargs)
gleam run
```

You should see output like:

```
INFO [<0.XX.0>] Starting Chronicle...
INFO [<0.XX.0>] Transport: RabbitMQ
INFO [<0.XX.0>] Connecting to RabbitMQ at localhost:5672
INFO [<0.XX.0>] Connected to RabbitMQ
INFO [<0.XX.0>] Channel opened
INFO [<0.XX.0>] Queue declared: chronicle.events
INFO [<0.XX.0>] Gateway started: RabbitMQ (chronicle.events)
```

## Configuration Options

| Variable | Default | Description |
|----------|---------|-------------|
| `CHRONICLE_TRANSPORT` | `otp` | Transport backend: `otp` or `rabbitmq` |
| `RABBITMQ_HOST` | `localhost` | RabbitMQ server hostname |
| `RABBITMQ_PORT` | `5672` | AMQP port |
| `RABBITMQ_USER` | `guest` | Username |
| `RABBITMQ_PASS` | `guest` | Password |
| `RABBITMQ_VHOST` | `/` | Virtual host |
| `RABBITMQ_QUEUE` | `chronicle.events` | Queue name |

## Verifying the Setup

### Send a Test Event

```bash
just ingest
```

### View the Queue in Management UI

1. Open [http://localhost:15672](http://localhost:15672)
2. Go to **Queues** tab
3. Find `chronicle.events`
4. You can see:
   - Message count
   - Consumer count
   - Message rates

### List Events

```bash
just list-events
```

## Architecture with RabbitMQ

```
                    ┌─────────────────────────────────────┐
                    │           HTTP API                  │
                    │         POST /events                │
                    └───────────────┬─────────────────────┘
                                    │
                                    ▼
                    ┌─────────────────────────────────────┐
                    │       Messaging Gateway             │
                    │     (gateway.gleam)                 │
                    │                                     │
                    │  Abstracts OTP vs RabbitMQ          │
                    └───────────────┬─────────────────────┘
                                    │
                                    ▼
                    ┌─────────────────────────────────────┐
                    │       RabbitMQ Backend              │
                    │      (rabbit.gleam)                 │
                    │                                     │
                    │  • Durable queue                    │
                    │  • Persistent delivery              │
                    │  • JSON serialization               │
                    └───────────────┬─────────────────────┘
                                    │ AMQP
                                    ▼
                    ┌─────────────────────────────────────┐
                    │         RabbitMQ Server             │
                    │     chronicle.events queue          │
                    │                                     │
                    │  ┌────────────────────────────┐     │
                    │  │  [E1] [E2] [E3] [E4] ...   │     │
                    │  └────────────────────────────┘     │
                    └─────────────────────────────────────┘
```

## Benefits of RabbitMQ Mode

1. **Durability**: Messages survive broker restarts
2. **Distribution**: Producers and consumers can run on different machines
3. **Scalability**: Add more consumers without code changes
4. **Monitoring**: Built-in management UI for visibility
5. **Decoupling**: True asynchronous messaging between services

## Comparison: OTP vs RabbitMQ

| Feature | OTP Mode | RabbitMQ Mode |
|---------|----------|---------------|
| Setup | None | Requires RabbitMQ server |
| Distribution | Single process | Multiple machines |
| Durability | In-memory only | Persistent to disk |
| Performance | Very fast | Network overhead |
| Use case | Development, single node | Production, distributed |

## Troubleshooting

### Connection Refused

If you see "Connection refused", ensure RabbitMQ is running:

```bash
docker-compose ps
# Should show "Up" status

docker-compose logs rabbitmq
# Check for startup errors
```

### Queue Not Created

The queue is created automatically when Chronicle starts. If it's missing:

1. Check Chronicle logs for errors
2. Ensure the RabbitMQ user has permissions
3. Try restarting Chronicle

### Messages Not Being Consumed

In RabbitMQ mode, you need to run a separate consumer process. See the section on running producers and consumers independently.

## Cleanup

Stop and remove RabbitMQ:

```bash
docker-compose down
```

Remove all data:

```bash
docker-compose down -v
```

