-module(rabbit_qos_ffi).
-export([set_prefetch/2]).

%% Include AMQP client records
-include_lib("amqp_client/include/amqp_client.hrl").

%% Set the prefetch count (QoS) on a channel
%% This enables fair dispatch to competing consumers
set_prefetch(ChannelPid, PrefetchCount) ->
    case amqp_channel:call(ChannelPid, #'basic.qos'{prefetch_count = PrefetchCount}) of
        #'basic.qos_ok'{} ->
            {ok, nil};
        Error ->
            {error, {qos_failed, Error}}
    end.

