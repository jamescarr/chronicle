-module(auditor_gateway_ffi).
-export([get_node_name/0]).

%% Get the current Erlang node name as a binary string
get_node_name() ->
    atom_to_binary(node(), utf8).

