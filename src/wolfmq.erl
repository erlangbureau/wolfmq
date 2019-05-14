-module(wolfmq).

%% API
-export([create_exchange/2]).
-export([destroy_exchange/1]).
%-export([bind/2]).
%-export([unbind/2]).
-export([route/3]).
-export([process/2, process/3]). %% replace [push/2, push/3]
-export([get_queue_size/1]).
-export([get_queue_id/0]).

%% deprecated
-export([push/2, push/3]).  %% replaced by [process/2, process/3]
-export([size/1]).          %% replaced by [get_queue_size/1]
-export([queue_id/0]).      %% replaced by [get_queue_id/0]

-deprecated([push/2, push/3]).  %% replaced by [process/2, process/3]
-deprecated([size/1]).          %% replaced by [get_queue_size/1]
-deprecated([queue_id/0]).      %% replaced by [get_queue_id/0]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% begin deprecated API functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
push(QueueId, Task) ->
    push(QueueId, Task, #{}).

push(QueueId, Task, Opts) ->
    process(QueueId, Task, Opts).

size(QueueId) ->
    get_queue_size(QueueId).

queue_id() ->
    get_queue_id().
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% end deprecated functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% API
create_exchange({GroupId, _Id} = ExchangeId, RoutingFun) when is_atom(GroupId) ->
    case wolfmq_queues_catalog:is_existing(ExchangeId) of
        true    ->
            ok;
        false   ->
            Opts = #{type => exchange, routing_fun => RoutingFun},
            wolfmq_mgr:start_worker(ExchangeId, Opts)
    end;
create_exchange(ExchangeId, RoutingFun) ->
    create_exchange({exchanges, ExchangeId}, RoutingFun).

destroy_exchange(ExchangeId) ->
    wolfmq_mgr:stop_worker(ExchangeId).

%bind(ExchangeId, Queues) ->

%unbind(ExchangeId, Queues) ->


route({GroupId, _Id} = ExchangeId, RoutingKey, Task) when is_atom(GroupId) ->
    add_to_queue(ExchangeId, {RoutingKey, Task});
route(ExchangeId, RoutingKey, Task) ->
    route({exchanges, ExchangeId}, RoutingKey, Task).

process(QueueId, Task) ->
    process(QueueId, Task, #{}).

process({GroupId, _QId} = QueueId, Task, Opts) when is_atom(GroupId) ->
    _ = case wolfmq_queues_catalog:is_existing(QueueId) of
        true    ->
            ok;
        false   ->
            Opts = #{type => task_processor},
            wolfmq_mgr:start_worker(QueueId, Opts)
    end,
    add_to_queue(QueueId, Task);
process(QueueId, Task, Opts) ->
    process({queues, QueueId}, Task, Opts).

get_queue_size({GroupId, _QId} = QueueId) when is_atom(GroupId) ->
    case wolfmq_queues_catalog:is_existing(QueueId) of
        true ->
            Meta = wolfmq_queues_catalog:get_meta(QueueId),
            InternalQueueId = maps:get(internal_id, Meta),
            wolfmq_queue:size(InternalQueueId);
        false ->
            0
    end;
get_queue_size(QueueId) ->
    get_queue_size({queues, QueueId}).

%% call it only from inside the queue
get_queue_id() ->
    wolfmq_worker:get_queue_id().

%% internal
add_to_queue(ExternalQueueId, Msgs) when is_list(Msgs) ->
    _ = [add_to_queue(ExternalQueueId, Msg) || Msg <- Msgs],
    ok;
add_to_queue(ExternalQueueId, Msg) ->
    Meta            = wolfmq_queues_catalog:get_meta(ExternalQueueId),
    InternalQueueId = maps:get(internal_id, Meta),
    MsgNumber       = wolfmq_sequences:next(ExternalQueueId),
    IsEmpty         = wolfmq_queue:is_empty(InternalQueueId),
    true            = wolfmq_queue:enqueue(InternalQueueId, {MsgNumber, Msg}),
    case IsEmpty of
        true ->
            WorkerPid = maps:get(worker_pid, Meta),
            wolfmq_worker:force_processing(WorkerPid);
        false ->
            ok
    end.
