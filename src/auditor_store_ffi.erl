-module(auditor_store_ffi).
-export([init/0, insert/2, list_all/1, get/2]).

%% Initialize the ETS table
init() ->
    ets:new(audit_events, [set, public, {keypos, 2}]).

%% Insert an event into the table
%% Event is a Gleam record: {audit_event, Id, Actor, Action, ResourceType, ResourceId, Timestamp, Metadata}
insert(Table, Event) ->
    ets:insert(Table, Event).

%% List all events
list_all(Table) ->
    case ets:tab2list(Table) of
        Events when is_list(Events) -> Events;
        _ -> []
    end.

%% Get an event by ID
get(Table, Id) ->
    case ets:lookup(Table, Id) of
        [Event] -> {ok, Event};
        [] -> {error, nil}
    end.
