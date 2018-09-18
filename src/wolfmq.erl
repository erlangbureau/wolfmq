-module(wolfmq).

%% API
-export([push/2, push/3]).
-export([size/1]).

-compile({no_auto_import, [size/1]}).

%% API
push(QueueId, Msg) ->
    push(QueueId, Msg, #{}).

push({GroupId, _QId} = QueueId, Msg, Opts) when is_atom(GroupId) ->
    _ = case wolfmq_queues_catalog:is_existing(QueueId) of
        true    ->
            ok;
        false   ->
            ok = wolfmq_mgr:start_worker(QueueId, Opts),
            case wolfmq_sequences:is_existing(QueueId) of
                true    -> ok;
                false   -> wolfmq_sequences:create(QueueId)
            end
    end,
    add_to_queue(QueueId, Msg);
push(QueueId, Msg, Opts) ->
    push({wolfmq_default_group, QueueId}, Msg, Opts).

size({GroupId, _QId} = QueueId) when is_atom(GroupId) ->
    case wolfmq_queues_catalog:is_existing(QueueId) of
        true ->
            Meta = wolfmq_queues_catalog:get_meta(QueueId),
            InternalQueueId = maps:get(internal_id, Meta),
            wolfmq_queue:size(InternalQueueId);
        false ->
            0
    end;
size(QueueId) ->
    size({wolfmq_default_group, QueueId}).

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
