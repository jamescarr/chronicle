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
# Run (with Erlang node names for debugging)
# =============================================================================

# Erlang cookie for node communication
cookie := "chronicle_dev"

[group('run')]
run:
    ERL_FLAGS="-sname chronicle -setcookie {{cookie}}" CHRONICLE_TRANSPORT={{transport}} gleam run

[group('run')]
producer:
    ERL_FLAGS="-sname producer -setcookie {{cookie}}" CHRONICLE_TRANSPORT={{transport}} CHRONICLE_MODE=producer gleam run

[group('run')]
consumer:
    ERL_FLAGS="-sname consumer1 -setcookie {{cookie}}" CHRONICLE_TRANSPORT={{transport}} CHRONICLE_MODE=consumer gleam run

# Start multiple consumers: `just consumers 3` or `just transport=rabbitmq consumers 5`
[group('run')]
consumers count="3":
    #!/usr/bin/env bash
    COUNT={{count}}
    echo "Starting $COUNT consumers..."
    for i in $(seq 1 $COUNT); do
        echo "Starting consumer$i..."
        ERL_FLAGS="-sname consumer$i -setcookie {{cookie}}" \
        CHRONICLE_TRANSPORT={{transport}} \
        CHRONICLE_MODE=consumer \
        gleam run &
        sleep 0.5  # Give each node time to register
    done
    echo ""
    echo "Started $COUNT consumers: consumer1 through consumer$COUNT"
    echo "Use 'just nodes' to verify, 'just kill' to stop all"
    wait

[group('run')]
watch:
    ERL_FLAGS="-sname chronicle -setcookie {{cookie}}" CHRONICLE_TRANSPORT={{transport}} watchexec -e gleam -r -- gleam run

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
    @echo ""
    @echo "Debug commands:"
    @echo "  just shell              - Connect to chronicle node"
    @echo "  just shell consumer1    - Connect to consumer1 node"
    @echo "  just ets                - List ETS tables on chronicle"
    @echo "  just events-dump        - Dump events from chronicle"
    @echo "  just nodes              - List running Erlang nodes"
