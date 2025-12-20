-module(auditor_routing_config_ffi).
-export([read_priv_file/1]).

%% Read a file from the priv directory
read_priv_file(Filename) ->
    case code:priv_dir(auditor) of
        {error, _} ->
            %% Fallback for development: try relative path
            case file:read_file(<<"priv/", Filename/binary>>) of
                {ok, Content} -> {ok, Content};
                {error, Reason} -> {error, atom_to_binary(Reason)}
            end;
        PrivDir ->
            Path = filename:join(PrivDir, Filename),
            case file:read_file(Path) of
                {ok, Content} -> {ok, Content};
                {error, Reason} -> {error, atom_to_binary(Reason)}
            end
    end.

