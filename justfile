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

# GET all events from the running server
test-get:
    @echo "Fetching events..."
    @curl -s http://localhost:8080/events | jq .

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

