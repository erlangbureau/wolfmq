-module(wolfmq).

%% API
-export([push/2]).

%% API
push(QueueId, Task) ->
    _ = case wolfmq_meta:is_existing_queue(QueueId) of
        true    -> ok;
        false   -> ok = wolfmq_mgr:start_worker(QueueId)
    end,
    add_to_queue(QueueId, Task).

%% internal
add_to_queue(QueueId, Tasks) when is_list(Tasks) ->
    _ = [add_to_queue(QueueId, Task)|| Task <- Tasks],
    ok;
add_to_queue(QueueId, Task) ->
    EtsId       = wolfmq_meta:get(QueueId, ets_id),
    TaskNumber  = wolfmq_meta:get(QueueId, next_task_number),
    WorkerPid   = wolfmq_meta:get(QueueId, worker_pid),
    IsEmpty     = wolfmq_queue:is_empty(EtsId),
    true = wolfmq_queue:enqueue(EtsId, {TaskNumber, Task}),
    case IsEmpty of
        true    -> wolfmq_worker:force_processing(WorkerPid);
        false   -> ok
    end.
