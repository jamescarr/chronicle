# Day 2 Pattern Diagrams Reference

Use this as a reference for sketching diagrams in Excalidraw.

---

## Pattern 1: Messaging Gateway

**Purpose:** Hide messaging infrastructure behind a clean interface. Application code doesn't know if it's using OTP or RabbitMQ.

**Visual Structure:**
```
┌─────────────────┐
│   Application   │
│    (Router)     │
└────────┬────────┘
         │
         │ gateway.send_event()
         ▼
┌─────────────────┐
│    Messaging    │
│    Gateway      │  ← Opaque type
│   (gateway.gleam)│
└────────┬────────┘
         │
    ┌────┴────┐
    │         │
    ▼         ▼
┌───────┐ ┌───────┐
│  OTP  │ │Rabbit │
│Channel│ │  MQ   │
└───────┘ └───────┘
```

**Key Elements:**
- Application layer (yellow box)
- Gateway layer (green box with "opaque" label)
- Two transport options (grey boxes, one active)
- Arrow showing `send_event()` call
- Dashed line showing configuration switch

**Chronicle Implementation:**
- `Gateway` opaque type with `OtpGateway` and `RabbitGateway` variants
- `gateway.send_event()` - same API regardless of transport
- `CHRONICLE_TRANSPORT` env var selects transport

---

## Pattern 2: Message Endpoint

**Purpose:** Separate sending (producer) from receiving (consumer). They can run independently.

**Visual Structure:**
```
┌─────────────────────────────────────────────────────────────┐
│                                                             │
│  ┌─────────────┐                      ┌─────────────┐      │
│  │  Sending    │                      │  Receiving  │      │
│  │  Endpoint   │                      │  Endpoint   │      │
│  │  (Producer) │                      │  (Consumer) │      │
│  └──────┬──────┘                      └──────┬──────┘      │
│         │                                    │             │
│         │ send()                    receive()│             │
│         ▼                                    ▼             │
│  ┌────────────────────────────────────────────────┐        │
│  │              Message Channel                    │        │
│  │         (OTP Subject / RabbitMQ Queue)         │        │
│  └────────────────────────────────────────────────┘        │
│                                                             │
└─────────────────────────────────────────────────────────────┘
                    Messaging System
```

**Key Elements:**
- Producer endpoint (left side, with HTTP icon)
- Consumer endpoint (right side, with Store icon)
- Channel connecting them (pipe in the middle)
- Labels: "Can run on different machines"
- Three modes: Full, Producer-only, Consumer-only

**Chronicle Implementation:**
- `CHRONICLE_MODE=full` - both producer and consumer
- `CHRONICLE_MODE=producer` - HTTP server only, publishes to channel
- `CHRONICLE_MODE=consumer` - no HTTP, subscribes to channel
- `gateway.start_consumers()` abstracts consumer lifecycle

---

## Pattern 3: Pipes and Filters

**Purpose:** Process events through a sequence of composable, independent steps.

**Visual Structure:**
```
     Incoming Event
          │
          ▼
    ┌───────────┐
    │ Validate  │ ──→ Reject (422)
    │ Required  │
    └─────┬─────┘
          │ Continue
          ▼
    ┌───────────┐
    │   Trim    │
    │  Fields   │
    └─────┬─────┘
          │
          ▼
    ┌───────────┐
    │ Normalize │
    │   Actor   │
    └─────┬─────┘
          │
          ▼
    ┌───────────┐
    │    Add    │
    │Correlation│
    │    ID     │
    └─────┬─────┘
          │
          ▼
    Processed Event
          │
          ▼
      Gateway
```

**Key Elements:**
- Each filter is a box
- Arrows connect filters (the "pipes")
- Show Continue path (down) and Reject path (sideways)
- Label each filter with its purpose
- Input event at top, output event at bottom

**Filter Results:**
- `Continue(event)` - pass to next filter
- `Reject(reason)` - stop with error
- `Skip(reason)` - silently drop

**Chronicle Filters:**
1. `validate_required()` - actor, action, resource_type non-empty
2. `trim_fields()` - remove whitespace
3. `normalize_actor()` - lowercase
4. `add_correlation_id()` - generate UUID if missing

---

## Pattern 4: Entity Enrichment (Read-Time)

**Purpose:** Hydrate events with current entity data when queried, not when stored.

**Visual Structure - Write Path:**
```
POST /events
     │
     │  { actor, action, entity_key: "org:acme" }
     ▼
┌─────────────────┐
│    Ingestion    │
│    Pipeline     │
│  (no enrichment)│
└────────┬────────┘
         │
         │  Store with entity_key reference
         ▼
┌─────────────────┐
│   Event Store   │
│ { entity_key:   │
│   "org:acme" }  │
└─────────────────┘
```

**Visual Structure - Read Path:**
```
GET /events
     │
     ▼
┌─────────────────┐
│   Event Store   │
└────────┬────────┘
         │
         │  For each event
         ▼
┌─────────────────┐     ┌─────────────────┐
│   Hydration     │ ←── │  Entity Store   │
│                 │     │  { org:acme:    │
│ if entity_key:  │     │    tier: "gold" │
│   lookup entity │     │    name: "Acme" │
│   merge metadata│     │  }              │
└────────┬────────┘     └─────────────────┘
         │
         │  Enriched event
         ▼
JSON Response
{ metadata: { tier: "gold", name: "Acme" } }
```

**Key Insight:**
- `entity_key` is like a foreign key
- Join happens at query time, not insert time
- Update entity → all historical events reflect new data

---

## Chronicle Day 2 Full Architecture

**Combined View:**
```
                         ┌─────────────────┐
                         │  HTTP Request   │
                         │  POST /events   │
                         └────────┬────────┘
                                  │
                                  ▼
┌──────────────────────────────────────────────────────────┐
│                    Ingestion Pipeline                     │
│  [Validate] → [Trim] → [Normalize] → [Add Correlation]   │
└──────────────────────────┬───────────────────────────────┘
                           │
                           ▼
              ┌─────────────────────────┐
              │    Messaging Gateway    │  ← Pattern 1
              │    (Opaque Transport)   │
              └────────────┬────────────┘
                           │
              ┌────────────┴────────────┐
              │                         │
              ▼                         ▼
       ┌───────────┐             ┌───────────┐
       │    OTP    │             │ RabbitMQ  │
       │  Channel  │             │   Queue   │
       └─────┬─────┘             └─────┬─────┘
              │                         │
              └────────────┬────────────┘
                           │
                           ▼
              ┌─────────────────────────┐
              │   Message Endpoint(s)   │  ← Pattern 2
              │      (Consumers)        │
              └────────────┬────────────┘
                           │
                           ▼
              ┌─────────────────────────┐
              │      Event Store        │
              │        (ETS)            │
              └─────────────────────────┘
                           │
                           │ GET /events
                           ▼
              ┌─────────────────────────┐     ┌──────────────┐
              │    Read-Time Hydration  │ ←── │ Entity Store │
              └────────────┬────────────┘     └──────────────┘
                           │
                           ▼
              ┌─────────────────────────┐
              │    Enriched Response    │
              └─────────────────────────┘
```

---

## Color Suggestions for Excalidraw

| Component | Color | Notes |
|-----------|-------|-------|
| Application/HTTP layer | Yellow | User-facing |
| Gateway | Green | Abstraction layer |
| OTP transport | Blue | In-process |
| RabbitMQ transport | Orange | Distributed |
| Pipeline filters | Light green | Processing |
| Stores (Event/Entity) | Grey | Persistence |
| Arrows | Black | Data flow |
| Config switch | Dashed purple | Environment-driven |

---

## Icons/Symbols

- **HTTP endpoint**: Globe or server icon
- **Gateway**: Door/gate icon
- **Filter**: Funnel icon
- **Channel/Queue**: Pipe or tube
- **Consumer**: Person or worker icon
- **Store**: Database cylinder
- **Entity**: Tag or label icon

