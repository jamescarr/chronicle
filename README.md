# Chronicle - Audit Logging System

A demonstration of Enterprise Integration Patterns implemented in Gleam, showcasing **Point-to-Point Channel** with **Competing Consumers**.

## Architecture

```
                    ┌─────────────────────────────────┐
                    │         HTTP Requests           │
                    │       POST /events (JSON)       │
                    └───────────────┬─────────────────┘
                                    │
                                    ▼
                    ┌─────────────────────────────────┐
                    │           API Layer             │
                    │         (router.gleam)          │
                    │                                 │
                    │  • Parse JSON                   │
                    │  • Generate ID & timestamp      │
                    │  • Send to channel              │
                    └───────────────┬─────────────────┘
                                    │
                                    ▼ channel.send(event)
                    ┌─────────────────────────────────┐
                    │     Point-to-Point Channel      │
                    │        (channel.gleam)          │
                    │                                 │
                    │  OTP Actor acting as a queue:   │
                    │  • FIFO ordering                │
                    │  • Each message → ONE consumer  │
                    │                                 │
                    │  [E1] → [E2] → [E3] → [E4]      │
                    └───────────────┬─────────────────┘
                                    │
                 ┌──────────────────┼──────────────────┐
                 │                  │                  │
                 ▼                  ▼                  ▼
        ┌─────────────┐    ┌─────────────┐    ┌─────────────┐
        │ Consumer 1  │    │ Consumer 2  │    │ Consumer 3  │
        │  <0.123.0>  │    │  <0.124.0>  │    │  <0.125.0>  │
        └──────┬──────┘    └──────┬──────┘    └──────┬──────┘
               │                  │                  │
               │    Competing Consumers Pattern      │
               │    Each races to grab messages      │
               │                  │                  │
               └──────────────────┼──────────────────┘
                                  │
                                  ▼ store.insert(event)
                    ┌─────────────────────────────────┐
                    │           ETS Store             │
                    │         (store.gleam)           │
                    │                                 │
                    │  In-memory Erlang Term Storage  │
                    │  • Concurrent read/write safe   │
                    │  • Keyed by event ID            │
                    └─────────────────────────────────┘
```

## Enterprise Integration Patterns

Chronicle demonstrates two patterns from *Enterprise Integration Patterns* by Hohpe & Woolf:

### Point-to-Point Channel

> A Point-to-Point Channel ensures that only one receiver consumes any given message.

- The **Channel** actor is our Point-to-Point Channel
- Messages (audit events) are queued in FIFO order
- Each message is delivered to exactly **one** consumer
- This guarantees no duplicate processing

### Competing Consumers

> Multiple consumers compete to receive messages from a single channel, enabling parallel processing.

- **3 consumer actors** poll the same channel
- They **race** to grab each message
- Load is **distributed** across consumers
- All write to the **same store** (ETS handles concurrency)

## Process-per-Request Model

Chronicle runs on the BEAM (Erlang VM), which spawns a **new lightweight process for each HTTP request**. This is visible in the logs:

```
INFO [<0.82.0>] Starting Chronicle...
INFO [<0.82.0>] Starting consumer: consumer-1
INFO [<0.82.0>] Starting consumer: consumer-2
INFO [<0.82.0>] Starting consumer: consumer-3
INFO [<0.82.0>] Started 3 competing consumers

INFO [<0.141.0>] Queued event abc-123...
INFO [<0.123.0>] [consumer-1] Processing abc-123 - create
INFO [<0.142.0>] Queued event def-456...
INFO [<0.124.0>] [consumer-2] Processing def-456 - update   ← different consumer!
INFO [<0.143.0>] Queued event ghi-789...
INFO [<0.125.0>] [consumer-3] Processing ghi-789 - delete   ← load distributed!
```

Each PID (e.g., `<0.123.0>`) identifies a unique process:

| Process | Role | Lifecycle |
|---------|------|-----------|
| `<0.82.0>` | Main application | Persistent |
| `<0.123.0>` | Consumer 1 | Persistent |
| `<0.124.0>` | Consumer 2 | Persistent |
| `<0.125.0>` | Consumer 3 | Persistent |
| `<0.141.0>`, `<0.142.0>`, ... | HTTP request handlers | Per-request |

This architecture provides:
- **Isolation** - if one request crashes, others are unaffected
- **Concurrency** - thousands of requests handled simultaneously
- **Fault tolerance** - embraces the "let it crash" philosophy
- **Scalability** - add more consumers to increase throughput

## Quick Start

### Prerequisites

- [Gleam](https://gleam.run/getting-started/installing/) 1.0+
- [Erlang/OTP](https://www.erlang.org/downloads) 26+
- [just](https://github.com/casey/just) (optional, for convenience commands)

### Running

```bash
# Using just (recommended)
just deps      # Install dependencies
just build     # Build the project
just run       # Start the server

# Or manually
gleam deps download
gleam build
gleam run
```

The server starts at `http://localhost:8080`.

## Testing

### Using just commands

```bash
# Start the server in one terminal
just run

# In another terminal:
just health        # Check server health
just ingest        # Send a random audit event
just ingest        # Send another (different random data each time)
just list-events   # View all stored events
```

### Manual testing with curl

```bash
# Health check
curl http://localhost:8080/health

# Create an audit event
curl -X POST http://localhost:8080/events \
  -H "Content-Type: application/json" \
  -d '{
    "actor": "alice@example.com",
    "action": "create",
    "resource_type": "document",
    "resource_id": "doc-123"
  }'

# List all events
curl http://localhost:8080/events
```

### Running automated tests

```bash
just test
# or
gleam test
```

This runs the end-to-end test suite which:
1. Starts a test server on port 9999
2. Creates events via the API
3. Verifies they appear in the list endpoint
4. Tests error handling for invalid JSON

## API Endpoints

| Method | Path | Description |
|--------|------|-------------|
| `GET` | `/health` | Health check |
| `POST` | `/events` | Create audit event |
| `GET` | `/events` | List all events |

### POST /events

**Request:**
```json
{
  "actor": "user@example.com",
  "action": "create",
  "resource_type": "document",
  "resource_id": "doc-123"
}
```

**Response (202 Accepted):**
```json
{
  "status": "accepted",
  "id": "550e8400-e29b-41d4-a716-446655440000"
}
```

### GET /events

**Response (200 OK):**
```json
[
  {
    "id": "550e8400-e29b-41d4-a716-446655440000",
    "actor": "user@example.com",
    "action": "create",
    "resource_type": "document",
    "resource_id": "doc-123",
    "timestamp": "2025-12-16T07:05:05.452Z"
  }
]
```

## Project Structure

```
src/
├── auditor.gleam           # Main entry point, starts all processes
├── auditor/
│   ├── event.gleam         # Audit event type (the "Message")
│   ├── channel.gleam       # Point-to-Point Channel (OTP Actor)
│   ├── consumer.gleam      # Consumer (OTP Actor)
│   ├── store.gleam         # ETS storage interface
│   ├── router.gleam        # HTTP router (Wisp)
│   └── log.gleam           # Logging with process IDs
└── auditor_store_ffi.erl   # Erlang FFI for ETS operations
```

## Available just commands

```bash
just              # Show all available commands
just bootstrap    # Clean, install deps, build
just build        # Build the project
just check        # Format, build, and test
just clean        # Remove build artifacts
just deps         # Install dependencies
just dev          # Install, build, and run
just fmt          # Format code
just health       # Check server health
just ingest       # Send a random event to the server
just flood 20     # Send 20 events in parallel (test competing consumers)
just list-events  # List all events from the server
just run          # Start the server
just test         # Run tests
just watch        # Run with auto-reload (requires watchexec)
```

## Dependencies

- **gleam_stdlib** - Standard library
- **gleam_erlang** - Erlang interop
- **gleam_otp** - OTP actor support
- **gleam_http** - HTTP types
- **gleam_json** - JSON encoding/decoding
- **wisp** - Web framework
- **mist** - HTTP server
- **youid** - UUID generation
- **birl** - Date/time handling
- **logging** - Structured logging

## Part of Advent of Enterprise Integration Patterns

This is Day 1 of the [Advent of Enterprise Integration Patterns](https://jamescarr.github.io) series, exploring messaging patterns while building Chronicle, a production-ready audit logging system in Gleam.
