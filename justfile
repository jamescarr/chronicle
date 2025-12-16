# Chronicle - Audit Logging System
# Run `just` to see available commands

# Default: show available commands
default:
    @just --list

# Install dependencies
deps:
    gleam deps download

# Build the project
build:
    gleam build

# Run all tests
test:
    gleam test

# Run the server
run:
    gleam run

# Run the server in watch mode (rebuilds on changes)
watch:
    watchexec -e gleam -r -- gleam run

# Format code
fmt:
    gleam format

# Check formatting without making changes
fmt-check:
    gleam format --check

# Clean build artifacts
clean:
    rm -rf build

# Bootstrap: clean, install deps, and build
bootstrap: clean deps build

# Full check: format, build, and test
check: fmt build test

# Development setup: install deps, build, run
dev: deps build run

# POST a test event to the running server
test-post:
    @echo "Sending test event..."
    @curl -s -X POST http://localhost:8080/events \
        -H "Content-Type: application/json" \
        -d '{"actor":"test@example.com","action":"test","resource_type":"example","resource_id":"123"}' | jq .

# Ingest a random event
ingest:
    #!/usr/bin/env bash
    actors=("alice@acme.com" "bob@acme.com" "charlie@acme.com" "diana@acme.com" "eve@acme.com")
    actions=("create" "update" "delete" "view" "export" "archive" "restore")
    resources=("user" "document" "project" "invoice" "report" "team" "setting")
    
    actor=${actors[$RANDOM % ${#actors[@]}]}
    action=${actions[$RANDOM % ${#actions[@]}]}
    resource=${resources[$RANDOM % ${#resources[@]}]}
    resource_id=$(uuidgen 2>/dev/null || cat /proc/sys/kernel/random/uuid 2>/dev/null || echo "id-$RANDOM")
    
    echo "Ingesting: $actor $action $resource/$resource_id"
    curl -s -X POST http://localhost:8080/events \
        -H "Content-Type: application/json" \
        -d "{\"actor\":\"$actor\",\"action\":\"$action\",\"resource_type\":\"$resource\",\"resource_id\":\"$resource_id\"}" | jq .

# GET all events from the running server
test-get:
    @echo "Fetching events..."
    @curl -s http://localhost:8080/events | jq .

# List all events
list-events:
    @curl -s http://localhost:8080/events | jq .

# Flood the server with random events (demonstrates competing consumers)
flood count="20":
    #!/usr/bin/env bash
    actors=("alice@acme.com" "bob@acme.com" "charlie@acme.com" "diana@acme.com" "eve@acme.com")
    actions=("create" "update" "delete" "view" "export" "archive" "restore")
    resources=("user" "document" "project" "invoice" "report" "team" "setting")
    
    for i in $(seq 1 {{count}}); do
        actor=${actors[$RANDOM % ${#actors[@]}]}
        action=${actions[$RANDOM % ${#actions[@]}]}
        resource=${resources[$RANDOM % ${#resources[@]}]}
        resource_id=$(uuidgen 2>/dev/null || cat /proc/sys/kernel/random/uuid 2>/dev/null || echo "id-$RANDOM")
        
        # Fire and forget - don't wait for response
        curl -s -X POST http://localhost:8080/events \
            -H "Content-Type: application/json" \
            -d "{\"actor\":\"$actor\",\"action\":\"$action\",\"resource_type\":\"$resource\",\"resource_id\":\"$resource_id\"}" &
    done
    wait
    echo "Sent {{count}} events"

# Health check
health:
    @curl -s http://localhost:8080/health | jq .

# Show project info
info:
    @echo "Chronicle - Audit Logging System"
    @echo ""
    @echo "Gleam version:"
    @gleam --version
    @echo ""
    @echo "Project structure:"
    @ls -la src/auditor/

