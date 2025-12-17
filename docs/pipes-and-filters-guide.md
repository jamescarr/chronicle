# Pipes and Filters: Step-by-Step Guide

This guide walks through using Chronicle's Pipes and Filters implementation for event enrichment.

## Overview

The Pipes and Filters pattern processes events through a sequence of composable filters:

```
Event → [Validate] → [Trim] → [Normalize] → [Add Correlation] → [Enrich] → Gateway
```

Each filter transforms the event, and the enriched result is sent to the messaging gateway.

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

Events flow through these filters in order:

1. **validate_required()** - Ensures actor, action, resource_type are non-empty
2. **trim_fields()** - Removes leading/trailing whitespace
3. **normalize_actor()** - Lowercases actor email for consistency
4. **add_correlation_id()** - Adds UUID if not provided
5. **enrich_from_entity()** - Looks up entity_key and merges attributes

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

## Custom Pipelines

You can create custom filter pipelines in Gleam:

```gleam
import auditor/filters
import auditor/pipeline

// Custom pipeline with additional validation
let my_pipeline = pipeline.from_filters([
  filters.validate_required(),
  filters.validate_actor_email(),  // Must contain @
  filters.trim_fields(),
  filters.normalize_actor(),
  filters.normalize_action(),      // Also lowercase action
  filters.add_correlation_id(),
  filters.add_source("my-service"),
  filters.enrich_from_entity(entity_store),
  filters.log_event("[audit]"),
])

// Process an event
case pipeline.process(my_pipeline, event) {
  Ok(enriched) -> // Send to gateway
  Error(reason) -> // Handle rejection
}
```

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                         HTTP Request                            │
│  POST /events { actor, action, ..., entity_key }               │
└─────────────────────┬───────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Processing Pipeline                          │
│  ┌──────────┐ ┌──────┐ ┌───────────┐ ┌─────────┐ ┌──────────┐  │
│  │ Validate │→│ Trim │→│ Normalize │→│ Add CID │→│ Enrich   │  │
│  └──────────┘ └──────┘ └───────────┘ └─────────┘ └────┬─────┘  │
│                                                       │         │
│                                          ┌────────────┘         │
│                                          ▼                      │
│                                    ┌──────────┐                 │
│                                    │ Entity   │                 │
│                                    │ Store    │                 │
│                                    └──────────┘                 │
└─────────────────────┬───────────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────────┐
│                    Messaging Gateway                            │
│              (OTP Channel or RabbitMQ)                          │
└─────────────────────────────────────────────────────────────────┘
```

## Related Patterns

- **Message Channel**: Events flow through the gateway's channel
- **Messaging Gateway**: Abstracts OTP vs RabbitMQ transport
- **Message Endpoint**: Producer/Consumer separation
- **Competing Consumers**: Multiple consumers process events in parallel

