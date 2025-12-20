--- migration:up
-- Add event_type column for Datatype Channel routing
ALTER TABLE audit_events ADD COLUMN event_type VARCHAR(255);

-- Create index for efficient routing key lookups
CREATE INDEX idx_audit_events_event_type ON audit_events (event_type);

-- Backfill existing rows with derived event_type
UPDATE audit_events
SET event_type = LOWER(resource_type) || '.' || LOWER(action)
WHERE event_type IS NULL;
--- migration:down
DROP INDEX IF EXISTS idx_audit_events_event_type;
ALTER TABLE audit_events DROP COLUMN IF EXISTS event_type;
--- migration:end

