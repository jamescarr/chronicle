-module(auditor_invalid_message_ffi).
-export([reject/3, get_message/2, get_queue_depth/2]).

-include_lib("amqp_client/include/amqp_client.hrl").

%% Reject a message (nack)
reject({channel, ChannelPid}, DeliveryTag, Requeue) ->
    Reject = #'basic.reject'{
        delivery_tag = DeliveryTag,
        requeue = Requeue
    },
    try
        ok = amqp_channel:call(ChannelPid, Reject),
        {ok, nil}
    catch
        Class:Reason ->
            {error, iolist_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
    end.

%% Get a single message from a queue (for inspection)
get_message({channel, ChannelPid}, QueueName) ->
    Get = #'basic.get'{
        queue = QueueName,
        no_ack = false
    },
    try
        case amqp_channel:call(ChannelPid, Get) of
            {#'basic.get_ok'{
                delivery_tag = DeliveryTag,
                exchange = Exchange,
                routing_key = RoutingKey
            }, #amqp_msg{payload = Payload, props = Props}} ->
                %% Ack the message after reading
                Ack = #'basic.ack'{delivery_tag = DeliveryTag},
                amqp_channel:call(ChannelPid, Ack),
                
                %% Extract x-death header if present
                Headers = Props#'P_basic'.headers,
                {DeathCount, FirstDeathTime, OrigQueue} = extract_death_info(Headers),
                
                DeadLetter = {dead_letter,
                    Payload,
                    unknown,  % reason - not easily extractable
                    Exchange,
                    RoutingKey,
                    OrigQueue,
                    DeathCount,
                    FirstDeathTime
                },
                {ok, {some, DeadLetter}};
            #'basic.get_empty'{} ->
                {ok, none}
        end
    catch
        Class:Reason ->
            {error, iolist_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
    end.

%% Extract death information from x-death header
extract_death_info(undefined) ->
    {0, none, <<>>};
extract_death_info(Headers) ->
    case lists:keyfind(<<"x-death">>, 1, Headers) of
        {_, array, Deaths} when length(Deaths) > 0 ->
            {_, table, FirstDeath} = hd(Deaths),
            Count = case lists:keyfind(<<"count">>, 1, FirstDeath) of
                {_, long, C} -> C;
                _ -> 1
            end,
            Time = case lists:keyfind(<<"time">>, 1, FirstDeath) of
                {_, timestamp, T} -> {some, integer_to_binary(T)};
                _ -> none
            end,
            Queue = case lists:keyfind(<<"queue">>, 1, FirstDeath) of
                {_, longstr, Q} -> Q;
                _ -> <<>>
            end,
            {Count, Time, Queue};
        _ ->
            {0, none, <<>>}
    end.

%% Get the number of messages in a queue
get_queue_depth({channel, ChannelPid}, QueueName) ->
    Declare = #'queue.declare'{
        queue = QueueName,
        passive = true
    },
    try
        case amqp_channel:call(ChannelPid, Declare) of
            #'queue.declare_ok'{message_count = Count} ->
                {ok, Count};
            Other ->
                {error, iolist_to_binary(io_lib:format("Unexpected: ~p", [Other]))}
        end
    catch
        exit:{{{shutdown, {server_initiated_close, 404, _}}, _}, _} ->
            {error, <<"queue_not_found">>};
        Class:Reason ->
            {error, iolist_to_binary(io_lib:format("~p:~p", [Class, Reason]))}
    end.

