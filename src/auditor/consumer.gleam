//// Consumer - Message ingestion and storage
////
//// Handles the core logic of consuming audit events and storing them.
//// This module is transport-agnostic - it doesn't care if messages
//// come from OTP channels or RabbitMQ.
////
//// This module is also storage-agnostic - it uses the EventStore
//// abstraction, so it doesn't care if events are stored in ETS or PostgreSQL.
////
//// From Enterprise Integration Patterns:
//// > A Message Endpoint is a client of the messaging system that
//// > sends and receives messages.
////
//// This consumer acts as a Message Endpoint, receiving events and
//// persisting them to storage.

import auditor/event.{type AuditEvent}
import auditor/event_store.{type EventStore}
import auditor/log

// =============================================================================
// Types
// =============================================================================

/// Handler function type - processes an event
/// This abstraction allows different transports to use the same processing logic
pub type EventHandler =
  fn(AuditEvent) -> Nil

/// Consumer configuration
pub type ConsumerConfig {
  ConsumerConfig(name: String, store: EventStore)
}

// =============================================================================
// Event Processing
// =============================================================================

/// Create a handler function that logs and stores events
/// This is the factory function for creating transport-agnostic handlers
pub fn create_handler(config: ConsumerConfig) -> EventHandler {
  fn(event: AuditEvent) -> Nil {
    process_event(event, config)
  }
}

/// Process a single event - the core consumer logic
/// Logs the event and persists it to storage
pub fn process_event(event: AuditEvent, config: ConsumerConfig) -> Nil {
  log.info(
    "[" <> config.name <> "] Processing " <> event.id <> " - " <> event.action,
  )
  let _ = event_store.insert(config.store, event)
  Nil
}

/// Simplified processing function when you just have a store and name
pub fn ingest(event: AuditEvent, store: EventStore, consumer_name: String) -> Nil {
  log.info(
    "[" <> consumer_name <> "] Processing " <> event.id <> " - " <> event.action,
  )
  let _ = event_store.insert(store, event)
  Nil
}

