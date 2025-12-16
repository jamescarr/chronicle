# Chronicle - Audit Logging System

A demonstration of Enterprise Integration Patterns implemented in Gleam, specifically showcasing the **Point-to-Point Channel** pattern.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        HTTP Request                              │
│                    POST /events (JSON)                          │
└─────────────────────────────────────────────────────────────────┘
                               │
                               ▼
┌─────────────────────────────────────────────────────────────────┐
│                          API Layer                               │
│                         (router.gleam)                          │
│    - Parse JSON                                                 │
│    - Generate ID & timestamp                                    │
│    - Create AuditEvent                                          │
└─────────────────────────────────────────────────────────────────┘
                               │
                               ▼ actor.send(channel, event)
┌─────────────────────────────────────────────────────────────────┐
│                    Point-to-Point Channel                        │
│                      (channel.gleam)                            │
│                                                                 │
│    An OTP Actor that acts as a queue:                          │
│    - Receives messages via Send(event)                          │
│    - Delivers to ONE consumer via Receive(reply_to)             │
│    - FIFO ordering guaranteed                                   │
│                                                                 │
│    [Event1] → [Event2] → [Event3]                               │
└─────────────────────────────────────────────────────────────────┘
                               │
                               ▼ channel.receive(...)
┌─────────────────────────────────────────────────────────────────┐
│                         Consumer                                 │
│                      (consumer.gleam)                           │
│                                                                 │
│    - Polls channel for events                                   │
│    - Writes to ETS store                                        │
│    - Single consumer = point-to-point                           │
└─────────────────────────────────────────────────────────────────┘
                               │
                               ▼ store.insert(...)
┌─────────────────────────────────────────────────────────────────┐
│                        ETS Store                                 │
│                       (store.gleam)                             │
│                                                                 │
│    In-memory Erlang Term Storage                                │
│    - Fast reads and writes                                      │
│    - Keyed by event ID                                          │
└─────────────────────────────────────────────────────────────────┘
```

## Enterprise Integration Pattern: Point-to-Point Channel

From the book *Enterprise Integration Patterns* by Gregor Hohpe and Bobby Woolf:

> A Point-to-Point Channel ensures that only one receiver consumes any given message.

In Chronicle:
- The **Channel** actor is our Point-to-Point Channel
- Messages (audit events) are queued in FIFO order
- Each message is delivered to exactly **one** consumer
- This guarantees no duplicate processing

## Running

```bash
# Install dependencies
gleam deps download

# Build
gleam build

# Run
gleam run
```

## API Endpoints

### Health Check
```bash
curl http://localhost:8080/health
# {"status":"healthy"}
```

### Create Audit Event
```bash
curl -X POST http://localhost:8080/events \
  -H "Content-Type: application/json" \
  -d '{
    "actor": "user@example.com",
    "action": "create",
    "resource_type": "document",
    "resource_id": "doc-123"
  }'
# {"status":"accepted","id":"<generated-uuid>"}
```

### List Events
```bash
curl http://localhost:8080/events
# [{"id":"...","actor":"...","action":"...","resource_type":"...","resource_id":"...","timestamp":"..."}]
```

## Project Structure

```
src/
├── auditor.gleam           # Main entry point
├── auditor/
│   ├── event.gleam         # Audit event type
│   ├── channel.gleam       # Point-to-Point Channel (OTP Actor)
│   ├── consumer.gleam      # Consumer (OTP Actor)
│   ├── store.gleam         # ETS storage interface
│   └── router.gleam        # HTTP router (Wisp)
├── auditor_store_ffi.erl   # Erlang FFI for ETS
└── auditor_router_ffi.erl  # Erlang FFI for ID/timestamp generation
```

## Dependencies

- **gleam_stdlib** - Standard library
- **gleam_erlang** - Erlang interop
- **gleam_otp** - OTP actor support
- **gleam_http** - HTTP types
- **gleam_json** - JSON encoding/decoding
- **wisp** - Web framework
- **mist** - HTTP server

## Part of Advent of Enterprise Integration Patterns

This is Day 1 of the [Advent of Enterprise Integration Patterns](https://jamescarr.github.io) series, exploring messaging patterns while building Chronicle, a production-ready audit logging system in Gleam.
