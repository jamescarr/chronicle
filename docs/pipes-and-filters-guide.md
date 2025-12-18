# Pipes and Filters: Step-by-Step Guide

This guide walks through using Chronicle's Pipes and Filters implementation for event processing and entity enrichment.

## Overview

The Pipes and Filters pattern processes events through a sequence of composable filters. Chronicle separates concerns between **write-time** and **read-time** processing:

**Ingestion Pipeline (Write Time):**
```
Event → [Validate] → [Trim] → [Normalize] → [Add Correlation] → Gateway → Store
```

**Hydration (Read Time):**
```
Store → [Lookup entity_key] → [Merge entity attributes] → Response
```

This separation ensures events always reflect **current** entity data when queried, not stale data from ingestion time.

## Quick Start

### 1. Start Chronicle

```bash
just run
```

### 2. Register an Entity

Entities provide metadata for enriching events. Register one via the API:

```bash
curl -X POST http://localhost:8080/entities \
  -H "Content-Type: application/json" \
  -d '{
    "key": "org:acme",
    "name": "Acme Corporation",
    "attributes": {
      "tier": "enterprise",
      "region": "us-west",
      "contact": "admin@acme.com"
    }
  }'
```

Response:
```json
{"status": "created", "key": "org:acme"}
```

### 3. Create an Event with Entity Reference

Include `entity_key` to trigger enrichment:

```bash
curl -X POST http://localhost:8080/events \
  -H "Content-Type: application/json" \
  -d '{
    "actor": "  Alice@ACME.COM  ",
    "action": "create",
    "resource_type": "document",
    "resource_id": "doc-123",
    "entity_key": "org:acme"
  }'
```

Response:
```json
{"status": "accepted", "id": "evt-uuid"}
```

### 4. View the Enriched Event

```bash
curl http://localhost:8080/events | jq
```

```json
[
  {
    "id": "evt-uuid",
    "actor": "alice@acme.com",
    "action": "create",
    "resource_type": "document",
    "resource_id": "doc-123",
    "timestamp": "2025-12-17T...",
    "correlation_id": "auto-generated-uuid",
    "entity_key": "org:acme",
    "metadata": {
      "entity_name": "Acme Corporation",
      "tier": "enterprise",
      "region": "us-west",
      "contact": "admin@acme.com"
    }
  }
]
```

Notice:
- `actor` was trimmed and lowercased
- `correlation_id` was auto-generated
- `metadata` contains entity attributes

## Entity Management

### List All Entities

```bash
curl http://localhost:8080/entities | jq
```

### Get Single Entity

```bash
curl http://localhost:8080/entities/org:acme | jq
```

### Delete Entity

```bash
curl -X DELETE http://localhost:8080/entities/org:acme
```

## Event Fields

### Required Fields

| Field | Description |
|-------|-------------|
| `actor` | Who performed the action (email) |
| `action` | What action was performed |
| `resource_type` | Type of resource acted upon |
| `resource_id` | Identifier of the resource |

### Optional Enrichment Fields

| Field | Description |
|-------|-------------|
| `entity_key` | Reference to registered entity for metadata enrichment |
| `correlation_id` | Custom correlation ID (auto-generated if omitted) |

## Pipeline Processing

### Ingestion Pipeline (Write Time)

When events are created, they flow through the ingestion pipeline:

1. **validate_required()** - Ensures actor, action, resource_type are non-empty
2. **trim_fields()** - Removes leading/trailing whitespace
3. **normalize_actor()** - Lowercases actor email for consistency
4. **add_correlation_id()** - Adds UUID if not provided

The event is then stored with its `entity_key` reference (if provided), but **not** enriched.

### Hydration (Read Time)

When events are queried via `GET /events`, each event is hydrated:

1. Check if event has an `entity_key`
2. Look up the entity in the entity store
3. Merge entity attributes into event metadata
4. Return the enriched event

**Why read-time enrichment?** If you update an entity (e.g., customer upgrades their tier), all their historical events automatically reflect the new data. No need to backfill!

### Validation Errors

If validation fails, you'll get HTTP 422:

```bash
curl -X POST http://localhost:8080/events \
  -H "Content-Type: application/json" \
  -d '{"actor": "", "action": "create", "resource_type": "doc", "resource_id": "1"}'
```

```json
{
  "error": "validation_failed",
  "reason": "Rejected: actor is required"
}
```

## Example: Multi-Tenant Audit Trail

### Setup Entities for Tenants

```bash
# Enterprise tenant
curl -X POST http://localhost:8080/entities -H "Content-Type: application/json" \
  -d '{"key": "tenant:acme", "name": "Acme Corp", "attributes": {"tier": "enterprise", "sla": "99.9%"}}'

# Starter tenant
curl -X POST http://localhost:8080/entities -H "Content-Type: application/json" \
  -d '{"key": "tenant:startup", "name": "Cool Startup", "attributes": {"tier": "starter", "sla": "99%"}}'
```

### Create Events with Tenant Context

```bash
# Enterprise user action
curl -X POST http://localhost:8080/events -H "Content-Type: application/json" \
  -d '{"actor": "ceo@acme.com", "action": "export", "resource_type": "report", "resource_id": "q4-financials", "entity_key": "tenant:acme"}'

# Startup user action
curl -X POST http://localhost:8080/events -H "Content-Type: application/json" \
  -d '{"actor": "founder@startup.io", "action": "create", "resource_type": "project", "resource_id": "mvp", "entity_key": "tenant:startup"}'
```

### Query Events

Events are now enriched with tenant metadata:

```bash
curl http://localhost:8080/events | jq '.[] | {actor, action, tier: .metadata.tier, sla: .metadata.sla}'
```

### Update Entity, Re-Query Events

Here's the magic of read-time enrichment. Update an entity:

```bash
# Acme upgrades to platinum tier
curl -X POST http://localhost:8080/entities -H "Content-Type: application/json" \
  -d '{"key": "tenant:acme", "name": "Acme Corp", "attributes": {"tier": "platinum", "sla": "99.99%"}}'
```

Query the same events again:

```bash
curl http://localhost:8080/events | jq '.[] | select(.entity_key == "tenant:acme") | {actor, tier: .metadata.tier}'
```

Output now shows `"tier": "platinum"` even though the event was created when they were "enterprise"!

## Custom Pipelines

You can create custom filter pipelines in Gleam:

```gleam
import auditor/filters
import auditor/pipeline

// Custom ingestion pipeline with additional validation
let my_pipeline = pipeline.from_filters([
  filters.validate_required(),
  filters.validate_actor_email(),  // Must contain @
  filters.trim_fields(),
  filters.normalize_actor(),
  filters.normalize_action(),      // Also lowercase action
  filters.add_correlation_id(),
  filters.add_source("my-service"),
  filters.log_event("[audit]"),
])

// Process an event
case pipeline.process(my_pipeline, event) {
  Ok(processed) -> // Send to gateway (entity_key is stored, not enriched)
  Error(reason) -> // Handle rejection
}
```

### Write-Time Enrichment (Advanced)

If you need write-time enrichment for a specific use case (e.g., event archival where you want a snapshot), the `enrich_from_entity` filter is still available:

```gleam
// Custom pipeline with write-time enrichment (denormalizes entity data)
let snapshot_pipeline = pipeline.from_filters([
  filters.validate_required(),
  filters.trim_fields(),
  filters.enrich_from_entity(entity_store),  // Bakes in entity data
])
```

**Note:** This creates a snapshot of entity data at ingestion time. The stored event won't reflect future entity updates.

## Architecture

### Write Path (Ingestion)

```
┌─────────────────────────────────────────────────────────────────┐
│                         HTTP Request                            │
│  POST /events { actor, action, ..., entity_key }               │
└─────────────────────┬───────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────────┐
│              Ingestion Pipeline (Write Time)                    │
│  ┌──────────┐ ┌──────┐ ┌───────────┐ ┌─────────┐               │
│  │ Validate │→│ Trim │→│ Normalize │→│ Add CID │               │
│  └──────────┘ └──────┘ └───────────┘ └─────────┘               │
│                                                                 │
│  NOTE: No enrichment here! entity_key is stored as a reference │
└─────────────────────┬───────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Messaging Gateway                            │
│              (OTP Channel or RabbitMQ)                          │
└─────────────────────┬───────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────────┐
│                      Event Store (ETS)                          │
│     { id, actor, action, ..., entity_key, metadata: {} }       │
└─────────────────────────────────────────────────────────────────┘
```

### Read Path (Hydration)

```
┌─────────────────────────────────────────────────────────────────┐
│                      GET /events                                │
└─────────────────────┬───────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────────┐
│                      Event Store (ETS)                          │
│               Read events from storage                          │
└─────────────────────┬───────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────────┐
│                 Hydration (Read Time)                           │
│  ┌───────────────────────────────────────────────────────────┐ │
│  │  For each event with entity_key:                          │ │
│  │    1. Lookup entity by key                                │ │
│  │    2. Merge entity.attributes into event.metadata         │ │
│  └───────────────────────────────────────────────────────────┘ │
│                          │                                      │
│                          ▼                                      │
│                   ┌──────────┐                                  │
│                   │ Entity   │ ← Current entity data            │
│                   │ Store    │                                  │
│                   └──────────┘                                  │
└─────────────────────┬───────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────────┐
│                    JSON Response                                │
│     { ..., metadata: { entity_name, tier, region, ... } }      │
└─────────────────────────────────────────────────────────────────┘
```

## Related Patterns

- **Message Channel**: Events flow through the gateway's channel
- **Messaging Gateway**: Abstracts OTP vs RabbitMQ transport
- **Message Endpoint**: Producer/Consumer separation
- **Competing Consumers**: Multiple consumers process events in parallel

