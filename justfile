# Chronicle - Audit Logging System
# Run `just` to see available commands
#
# Composable variables - combine them freely:
#   just transport=rabbitmq run
#   just transport=rabbitmq role=security consumers 3
#   just store=ets run

# Transport adapter: "otp" (default) or "rabbitmq"
transport := env("CHRONICLE_TRANSPORT", "otp")

# Store backend: "postgres" (default) or "ets"
store := env("CHRONICLE_STORE", "postgres")

# Consumer role: "default" (all events) or specific roles from chronicle.toml
# e.g., "security", "analytics", "billing", "users"
role := env("CHRONICLE_CONSUMER_ROLE", "default")

# Event type for sending test events: "security", "user", "billing"
event := "security"

# PostgreSQL connection defaults
pg_host := env("POSTGRES_HOST", "localhost")
pg_port := env("POSTGRES_PORT", "5432")
pg_user := env("POSTGRES_USER", "chronicle")
pg_pass := env("POSTGRES_PASSWORD", "chronicle")
pg_db := env("POSTGRES_DATABASE", "chronicle")
database_url := env("DATABASE_URL", "postgresql://" + pg_user + ":" + pg_pass + "@" + pg_host + ":" + pg_port + "/" + pg_db)

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
# Run (with Erlang node names for debugging)
# =============================================================================

# Erlang cookie for node communication
cookie := "chronicle_dev"

[group('run')]
run:
    ERL_FLAGS="-sname chronicle -setcookie {{cookie}}" \
    CHRONICLE_TRANSPORT={{transport}} \
    CHRONICLE_STORE={{store}} \
    POSTGRES_HOST={{pg_host}} \
    POSTGRES_PORT={{pg_port}} \
    POSTGRES_USER={{pg_user}} \
    POSTGRES_PASSWORD={{pg_pass}} \
    POSTGRES_DATABASE={{pg_db}} \
    gleam run

[group('run')]
producer:
    ERL_FLAGS="-sname producer -setcookie {{cookie}}" \
    CHRONICLE_TRANSPORT={{transport}} \
    CHRONICLE_STORE={{store}} \
    POSTGRES_HOST={{pg_host}} \
    POSTGRES_PORT={{pg_port}} \
    POSTGRES_USER={{pg_user}} \
    POSTGRES_PASSWORD={{pg_pass}} \
    POSTGRES_DATABASE={{pg_db}} \
    CHRONICLE_MODE=producer gleam run

# Single consumer: just consumer (or just role=security consumer)
[group('run')]
consumer:
    ERL_FLAGS="-sname consumer_{{role}}_1 -setcookie {{cookie}}" \
    CHRONICLE_TRANSPORT={{transport}} \
    CHRONICLE_STORE={{store}} \
    CHRONICLE_MODE=consumer \
    CHRONICLE_CONSUMER_ROLE={{role}} \
    POSTGRES_HOST={{pg_host}} \
    POSTGRES_PORT={{pg_port}} \
    POSTGRES_USER={{pg_user}} \
    POSTGRES_PASSWORD={{pg_pass}} \
    POSTGRES_DATABASE={{pg_db}} \
    gleam run

# Multiple consumers with the same role:
#   just consumers 3
#   just transport=rabbitmq consumers 5
#   just transport=rabbitmq role=security consumers 2
#   just role=analytics consumers 1
[group('run')]
consumers count="3":
    #!/usr/bin/env bash
    COUNT={{count}}
    ROLE="{{role}}"
    echo "Starting $COUNT consumers (role: $ROLE, transport: {{transport}})..."
    for i in $(seq 1 $COUNT); do
        echo "Starting consumer_${ROLE}_$i..."
        ERL_FLAGS="-sname consumer_${ROLE}_$i -setcookie {{cookie}}" \
        CHRONICLE_TRANSPORT={{transport}} \
        CHRONICLE_STORE={{store}} \
        CHRONICLE_MODE=consumer \
        CHRONICLE_CONSUMER_ROLE={{role}} \
        POSTGRES_HOST={{pg_host}} \
        POSTGRES_PORT={{pg_port}} \
        POSTGRES_USER={{pg_user}} \
        POSTGRES_PASSWORD={{pg_pass}} \
        POSTGRES_DATABASE={{pg_db}} \
        gleam run &
        sleep 0.5  # Give each node time to register
    done
    echo ""
    echo "Started $COUNT $ROLE consumers"
    echo "Use 'just nodes' to verify, 'just kill' to stop all"
    wait

[group('run')]
watch:
    ERL_FLAGS="-sname chronicle -setcookie {{cookie}}" \
    CHRONICLE_TRANSPORT={{transport}} \
    CHRONICLE_STORE={{store}} \
    POSTGRES_HOST={{pg_host}} \
    POSTGRES_PORT={{pg_port}} \
    POSTGRES_USER={{pg_user}} \
    POSTGRES_PASSWORD={{pg_pass}} \
    POSTGRES_DATABASE={{pg_db}} \
    watchexec -e gleam -r -- gleam run

[group('run')]
dev: deps build run

# Full setup: start services, run migrations, then run the app
[group('run')]
setup: services-up
    @echo "Waiting for services to be ready..."
    @sleep 3
    @just migrate
    @echo "Setup complete! Run 'just run' to start the app"

# Start all infrastructure services
[group('run')]
services-up:
    docker-compose up -d
    @echo "Services starting..."
    @echo "PostgreSQL: postgresql://{{pg_user}}:{{pg_pass}}@{{pg_host}}:{{pg_port}}/{{pg_db}}"
    @echo "RabbitMQ: http://localhost:15672 (guest/guest)"

# Stop all infrastructure services
[group('run')]
services-down:
    docker-compose down

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
# Store Shortcuts
# =============================================================================

[group('store')]
ets-mode:
    CHRONICLE_STORE=ets gleam run

[group('store')]
postgres-mode:
    CHRONICLE_STORE=postgres \
    POSTGRES_HOST={{pg_host}} \
    POSTGRES_PORT={{pg_port}} \
    POSTGRES_USER={{pg_user}} \
    POSTGRES_PASSWORD={{pg_pass}} \
    POSTGRES_DATABASE={{pg_db}} \
    gleam run

[group('store')]
which-store:
    @echo "Current store: {{store}}"
    @echo "Database URL:  {{database_url}}"
    @echo ""
    @echo "To change, either:"
    @echo "  export CHRONICLE_STORE=ets"
    @echo "  just store=ets run"

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

# Send events slowly to see round-robin distribution
[group('api')]
flood-slow count="10":
    #!/usr/bin/env bash
    actors=("alice@acme.com" "bob@acme.com" "charlie@acme.com")
    actions=("create" "update" "delete")
    resources=("user" "document" "project")
    COUNT={{count}}
    
    echo "Sending $COUNT events with 500ms delay..."
    for i in $(seq 1 $COUNT); do
        actor=${actors[$RANDOM % ${#actors[@]}]}
        action=${actions[$RANDOM % ${#actions[@]}]}
        resource=${resources[$RANDOM % ${#resources[@]}]}
        
        echo -n "Event $i: "
        curl -s -X POST http://localhost:8080/events \
            -H "Content-Type: application/json" \
            -d "{\"actor\":\"$actor\",\"action\":\"$action\",\"resource_type\":\"$resource\",\"resource_id\":\"evt-$i\"}" \
            | jq -r '.id // .error'
        sleep 0.5
    done
    echo "Done"

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
rabbit-consumers:
    @echo "Consumer details:"
    @curl -s -u guest:guest http://localhost:15672/api/queues/%2F/chronicle.events 2>/dev/null \
        | jq '.consumer_details[] | {tag: .consumer_tag, prefetch: .prefetch_count, ack_required: .ack_required, port: .channel_details.peer_port}' \
        || echo "Queue not found"

[group('rabbitmq')]
rabbit-purge:
    @echo "Purging queue..."
    @curl -s -X DELETE -u guest:guest http://localhost:15672/api/queues/%2F/chronicle.events/contents 2>/dev/null \
        && echo "Queue purged" || echo "Failed to purge"

[group('rabbitmq')]
rabbit-clean:
    docker-compose down -v
    @echo "RabbitMQ data cleaned"

# =============================================================================
# Database Migrations (cigogne)
# =============================================================================

# Run all pending migrations
[group('db')]
migrate:
    DATABASE_URL={{database_url}} gleam run -m cigogne all

# Run next pending migration
[group('db')]
migrate-up:
    DATABASE_URL={{database_url}} gleam run -m cigogne up

# Run N pending migrations
[group('db')]
migrate-up-n count="1":
    DATABASE_URL={{database_url}} gleam run -m cigogne up --count {{count}}

# Rollback last migration
[group('db')]
migrate-down:
    DATABASE_URL={{database_url}} gleam run -m cigogne down

# Rollback N migrations
[group('db')]
migrate-down-n count="1":
    DATABASE_URL={{database_url}} gleam run -m cigogne down --count {{count}}

# Show migration status
[group('db')]
migrate-status:
    DATABASE_URL={{database_url}} gleam run -m cigogne show

# Show unapplied migrations
[group('db')]
migrate-pending:
    DATABASE_URL={{database_url}} gleam run -m cigogne print-unapplied

# Create a new migration file
[group('db')]
migrate-new name:
    gleam run -m cigogne new --name {{name}}

# Initialize cigogne config (creates priv/cigogne.toml)
[group('db')]
migrate-init:
    gleam run -m cigogne init

# =============================================================================
# PostgreSQL Infrastructure
# =============================================================================

# Start PostgreSQL (and RabbitMQ)
[group('postgres')]
pg-up:
    docker-compose up -d postgres
    @echo "PostgreSQL starting..."
    @echo "Connection: postgresql://{{pg_user}}:{{pg_pass}}@{{pg_host}}:{{pg_port}}/{{pg_db}}"

# Stop PostgreSQL
[group('postgres')]
pg-down:
    docker-compose stop postgres

# View PostgreSQL logs
[group('postgres')]
pg-logs:
    docker-compose logs -f postgres

# Connect to PostgreSQL CLI
[group('postgres')]
pg-shell:
    docker-compose exec postgres psql -U {{pg_user}} -d {{pg_db}}

# Reset PostgreSQL (WARNING: destroys all data)
[group('postgres')]
pg-reset:
    docker-compose down -v postgres
    docker-compose up -d postgres
    @echo "Waiting for PostgreSQL to start..."
    @sleep 3
    @just migrate
    @echo "PostgreSQL reset complete"

# Show table info
[group('postgres')]
pg-tables:
    docker-compose exec postgres psql -U {{pg_user}} -d {{pg_db}} -c '\dt'

# Count events in database
[group('postgres')]
pg-count:
    docker-compose exec postgres psql -U {{pg_user}} -d {{pg_db}} -c 'SELECT COUNT(*) FROM audit_events;'

# Show recent events
[group('postgres')]
pg-events count="10":
    docker-compose exec postgres psql -U {{pg_user}} -d {{pg_db}} -c 'SELECT id, actor, action, resource_type, timestamp FROM audit_events ORDER BY timestamp DESC LIMIT {{count}};'

# =============================================================================
# Debug - Erlang shell & ETS inspection
# =============================================================================

[group('debug')]
shell node="chronicle":
    @echo "Connecting to {{node}}@$(hostname -s)..."
    @echo "Use Ctrl+G then 'q' to quit (not Ctrl+C!)"
    erl -sname debug_$$ -setcookie {{cookie}} -remsh {{node}}@$(hostname -s)

[group('debug')]
ets node="chronicle":
    #!/usr/bin/env bash
    echo "ETS tables on {{node}}@$(hostname -s):"
    erl -sname ets_inspect_$$ -setcookie {{cookie}} -noshell -eval "
        case net_adm:ping('{{node}}@$(hostname -s)') of
            pong ->
                Tables = rpc:call('{{node}}@$(hostname -s)', ets, all, []),
                io:format(\"~nFound ~p tables:~n\", [length(Tables)]),
                lists:foreach(fun(T) ->
                    Info = rpc:call('{{node}}@$(hostname -s)', ets, info, [T]),
                    Name = proplists:get_value(name, Info),
                    Size = proplists:get_value(size, Info),
                    io:format(\"  ~p: ~p entries~n\", [Name, Size])
                end, Tables);
            pang ->
                io:format(\"Cannot connect to {{node}}@$(hostname -s)~n\")
        end,
        init:stop().
    "

[group('debug')]
events-dump node="chronicle":
    #!/usr/bin/env bash
    echo "Events from {{node}}@$(hostname -s):"
    erl -sname dump_$$ -setcookie {{cookie}} -noshell -eval "
        case net_adm:ping('{{node}}@$(hostname -s)') of
            pong ->
                Tables = rpc:call('{{node}}@$(hostname -s)', ets, all, []),
                % Find the audit_events table
                case lists:filter(fun(T) ->
                    Info = rpc:call('{{node}}@$(hostname -s)', ets, info, [T]),
                    proplists:get_value(name, Info) == audit_events
                end, Tables) of
                    [Table|_] ->
                        Events = rpc:call('{{node}}@$(hostname -s)', ets, tab2list, [Table]),
                        io:format(\"~nFound ~p events:~n\", [length(Events)]),
                        lists:foreach(fun(E) -> io:format(\"  ~p~n\", [E]) end, lists:sublist(Events, 10)),
                        case length(Events) > 10 of
                            true -> io:format(\"  ... and ~p more~n\", [length(Events) - 10]);
                            false -> ok
                        end;
                    [] ->
                        io:format(\"No audit_events table found~n\")
                end;
            pang ->
                io:format(\"Cannot connect to {{node}}@$(hostname -s)~n\")
        end,
        init:stop().
    "

[group('debug')]
nodes:
    @echo "Looking for Chronicle nodes..."
    @echo "Expected nodes: chronicle, producer, consumer1, consumer2, etc."
    @echo ""
    @epmd -names 2>/dev/null || echo "epmd not running (no Erlang nodes active)"

# =============================================================================
# Routing Configuration
# =============================================================================

# Show configured routes from chronicle.toml
[group('routing')]
show-routes:
    @cat priv/chronicle.toml | grep -A5 '\[\[routes\]\]'

# Show configured consumers from chronicle.toml
[group('routing')]
show-consumers:
    @cat priv/chronicle.toml | grep -A5 '\[\[consumers\]\]'

# Send a typed event for datatype channel testing
# Usage: just event=security send
#        just event=user send
#        just event=billing send
[group('routing')]
send:
    #!/usr/bin/env bash
    case "{{event}}" in
        security)
            echo "Sending security event..."
            PAYLOAD='{"actor":"test@example.com","action":"login","resource_type":"session","resource_id":"session-123","event_type":"security.login"}'
            ;;
        user)
            echo "Sending user event..."
            PAYLOAD='{"actor":"admin@example.com","action":"created","resource_type":"user","resource_id":"user-456","event_type":"user.created"}'
            ;;
        billing)
            echo "Sending billing event..."
            PAYLOAD='{"actor":"billing@example.com","action":"charge","resource_type":"invoice","resource_id":"inv-789","event_type":"billing.charge"}'
            ;;
        *)
            echo "Sending {{event}} event..."
            PAYLOAD='{"actor":"test@example.com","action":"test","resource_type":"{{event}}","resource_id":"id-'$RANDOM'","event_type":"{{event}}.test"}'
            ;;
    esac
    curl -s -X POST http://localhost:8080/events \
        -H "Content-Type: application/json" \
        -d "$PAYLOAD" | jq .

# List all RabbitMQ queues
[group('routing')]
list-queues:
    @echo "RabbitMQ queues:"
    @curl -s -u guest:guest http://localhost:15672/api/queues | jq '.[] | {name: .name, messages: .messages, consumers: .consumers}'

# List all RabbitMQ exchanges
[group('routing')]
list-exchanges:
    @echo "RabbitMQ exchanges:"
    @curl -s -u guest:guest http://localhost:15672/api/exchanges | jq '.[] | select(.name != "") | {name: .name, type: .type}'

# =============================================================================
# Info
# =============================================================================

[group('info')]
info:
    @echo "Chronicle - Audit Logging System"
    @echo ""
    @echo "Current Configuration:"
    @echo "  Transport: {{transport}}"
    @echo "  Store:     {{store}}"
    @echo "  Role:      {{role}}"
    @echo "  Database:  {{database_url}}"
    @echo ""
    @echo "Gleam version:"
    @gleam --version
    @echo ""
    @echo "Quick start:"
    @echo "  just setup              - Start services, run migrations"
    @echo "  just run                - Run full app (producer + consumer)"
    @echo "  just producer           - Run producer only (HTTP server)"
    @echo "  just consumer           - Run single consumer"
    @echo "  just consumers 3        - Run 3 consumers"
    @echo ""
    @echo "Composable variables (combine freely):"
    @echo "  just transport=rabbitmq run"
    @echo "  just transport=rabbitmq role=security consumers 3"
    @echo "  just role=analytics consumers 1"
    @echo "  just store=ets run"
    @echo ""
    @echo "Routing commands:"
    @echo "  just event=security send  - Send security event"
    @echo "  just event=user send      - Send user event"
    @echo "  just event=billing send   - Send billing event"
    @echo "  just show-routes          - Show configured routes"
    @echo "  just list-queues          - List RabbitMQ queues"
    @echo ""
    @echo "Migration commands:"
    @echo "  just migrate            - Run all pending migrations"
    @echo "  just migrate-status     - Show migration status"
    @echo ""
    @echo "Debug commands:"
    @echo "  just nodes              - List running Erlang nodes"
    @echo "  just kill               - Stop all Chronicle processes"
