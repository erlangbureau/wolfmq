-module(wolfmq_meta).

%% API
-export([init/0]).
-export([open_queue/2, close_queue/1]).
-export([is_existing_queue/1]).
-export([get/2]).

%% API
init() ->
    _ = ets:new(?MODULE, [public, named_table, {read_concurrency, true}]),
    ok.

open_queue(QueueId, {EtsId, WorkerPid}) ->
    true = ets:insert(?MODULE, {QueueId, EtsId, 0, WorkerPid}),
    ok.

close_queue(QueueId) ->
    true = ets:delete(?MODULE, QueueId),
    ok.

is_existing_queue(QueueId) ->
    case ets:lookup(?MODULE, QueueId) of
        [] -> false;
        [{_, _, _, WorkerPid}] -> is_process_alive(WorkerPid)
    end.

get(QueueId, ets_id) ->
    ets:lookup_element(?MODULE, QueueId, 2);
get(QueueId, next_task_number) ->
    ets:update_counter(?MODULE, QueueId, {3,1});
get(QueueId, worker_pid) ->
    ets:lookup_element(?MODULE, QueueId, 4).
