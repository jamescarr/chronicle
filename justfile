# Chronicle - Audit Logging System
# Run `just` to see available commands

# Transport adapter: "otp" (default) or "rabbitmq"
transport := env("CHRONICLE_TRANSPORT", "otp")

# Default: show available commands
default:
    @just --list --unsorted

# =============================================================================
# Build & Test
# =============================================================================

[group('build')]
deps:
    gleam deps download

[group('build')]
build:
    gleam build

[group('build')]
test:
    gleam test

[group('build')]
fmt:
    gleam format

[group('build')]
fmt-check:
    gleam format --check

[group('build')]
clean:
    rm -rf build

[group('build')]
bootstrap: clean deps build

[group('build')]
check: fmt build test

# =============================================================================
# Run
# =============================================================================

[group('run')]
run:
    CHRONICLE_TRANSPORT={{transport}} gleam run

[group('run')]
producer:
    CHRONICLE_TRANSPORT={{transport}} CHRONICLE_MODE=producer gleam run

[group('run')]
consumer:
    CHRONICLE_TRANSPORT={{transport}} CHRONICLE_MODE=consumer gleam run

[group('run')]
watch:
    CHRONICLE_TRANSPORT={{transport}} watchexec -e gleam -r -- gleam run

[group('run')]
dev: deps build run

[group('run')]
kill:
    @pkill -f "gleam run" 2>/dev/null || true
    @pkill -f beam.smp 2>/dev/null || true
    @lsof -ti:8080 | xargs kill -9 2>/dev/null || true
    @echo "Chronicle stopped"

# =============================================================================
# Transport Shortcuts
# =============================================================================

[group('transport')]
otp:
    CHRONICLE_TRANSPORT=otp gleam run

[group('transport')]
rabbit:
    CHRONICLE_TRANSPORT=rabbitmq gleam run

[group('transport')]
which-transport:
    @echo "Current transport: {{transport}}"
    @echo ""
    @echo "To change, either:"
    @echo "  export CHRONICLE_TRANSPORT=rabbitmq"
    @echo "  just transport=rabbitmq run"

# =============================================================================
# API Testing
# =============================================================================

[group('api')]
health:
    @curl -s http://localhost:8080/health | jq .

[group('api')]
post:
    @echo "Sending test event..."
    @curl -s -X POST http://localhost:8080/events \
        -H "Content-Type: application/json" \
        -d '{"actor":"test@example.com","action":"test","resource_type":"example","resource_id":"123"}' | jq .

[group('api')]
events:
    @curl -s http://localhost:8080/events | jq .

[group('api')]
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

[group('api')]
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
        
        curl -s -X POST http://localhost:8080/events \
            -H "Content-Type: application/json" \
            -d "{\"actor\":\"$actor\",\"action\":\"$action\",\"resource_type\":\"$resource\",\"resource_id\":\"$resource_id\"}" &
    done
    wait
    echo "Sent {{count}} events"

# =============================================================================
# RabbitMQ Infrastructure
# =============================================================================

[group('rabbitmq')]
rabbit-up:
    docker-compose up -d
    @echo "RabbitMQ starting..."
    @echo "Management UI: http://localhost:15672 (guest/guest)"

[group('rabbitmq')]
rabbit-down:
    docker-compose down

[group('rabbitmq')]
rabbit-logs:
    docker-compose logs -f rabbitmq

[group('rabbitmq')]
rabbit-status:
    @docker-compose ps
    @echo ""
    @echo "Queue status:"
    @curl -s -u guest:guest http://localhost:15672/api/queues/%2F/chronicle.events 2>/dev/null \
        | jq '{name: .name, messages: .messages, consumers: .consumers}' \
        || echo "Queue not found or RabbitMQ not running"

[group('rabbitmq')]
rabbit-clean:
    docker-compose down -v
    @echo "RabbitMQ data cleaned"

# =============================================================================
# Info
# =============================================================================

[group('info')]
info:
    @echo "Chronicle - Audit Logging System"
    @echo ""
    @echo "Current transport: {{transport}}"
    @echo ""
    @echo "Gleam version:"
    @gleam --version
    @echo ""
    @echo "Project structure:"
    @ls -la src/auditor/
