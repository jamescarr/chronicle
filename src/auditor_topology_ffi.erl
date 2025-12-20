-module(auditor_topology_ffi).
-export([declare_queue_with_dlx/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

%% Declare a queue with dead letter exchange argument
declare_queue_with_dlx({channel, ChannelPid}, QueueName, DeadLetterExchange) ->
    Args = [{<<"x-dead-letter-exchange">>, longstr, DeadLetterExchange}],
    Declare = #'queue.declare'{
        queue = QueueName,
        durable = true,
        exclusive = false,
        auto_delete = false,
        arguments = Args
    },
    try
        case amqp_channel:call(ChannelPid, Declare) of
            #'queue.declare_ok'{queue = Name} ->
                {ok, Name};
            Other ->
                {error, iolist_to_binary(io_lib:format("Unexpected response: ~p", [Other]))}
        end
    catch
        exit:{{{shutdown, {server_initiated_close, Code, Reason}}, _}, _} ->
            {error, iolist_to_binary(io_lib:format("Channel error ~p: ~s", [Code, Reason]))};
        Class:Reason:Stacktrace ->
            {error, iolist_to_binary(io_lib:format("~p:~p~n~p", [Class, Reason, Stacktrace]))}
    end.

