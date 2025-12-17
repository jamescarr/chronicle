-module(auditor_entity_store_ffi).
-export([init/0, put/2, get/2, delete/2, list_all/1, exists/2, count/1]).

%% Initialize the ETS table for entities
%% Entity is a Gleam record: {entity, Key, Name, Attributes}
%% Key position is 2 (the Key field)
init() ->
    ets:new(entities, [set, public, {keypos, 2}]).

%% Insert or update an entity
put(Table, Entity) ->
    ets:insert(Table, Entity).

%% Get an entity by key
get(Table, Key) ->
    case ets:lookup(Table, Key) of
        [Entity] -> {ok, Entity};
        [] -> {error, nil}
    end.

%% Delete an entity by key
delete(Table, Key) ->
    ets:delete(Table, Key).

%% List all entities
list_all(Table) ->
    case ets:tab2list(Table) of
        Entities when is_list(Entities) -> Entities;
        _ -> []
    end.

%% Check if an entity exists
exists(Table, Key) ->
    case ets:lookup(Table, Key) of
        [_Entity] -> true;
        [] -> false
    end.

%% Count entities
count(Table) ->
    ets:info(Table, size).

