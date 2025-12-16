-module(auditor_router_ffi).
-export([generate_id/0, get_timestamp/0]).

%% Generate a UUID-like ID
generate_id() ->
    Bytes = crypto:strong_rand_bytes(16),
    Hex = binary:encode_hex(Bytes),
    %% Format as UUID: 8-4-4-4-12
    <<A:8/binary, B:4/binary, C:4/binary, D:4/binary, E:12/binary>> = Hex,
    iolist_to_binary([A, "-", B, "-", C, "-", D, "-", E]).

%% Get current timestamp in ISO 8601 format
get_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                  [Year, Month, Day, Hour, Min, Sec])).
