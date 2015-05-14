-module(wolfmq_mgr).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([push/2]).
-export([open_queue/2, close_queue/1]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

push(QueueId, Task) ->
    _ = case is_existing_queue(QueueId) of
        true ->
            ok;
        false ->
            ok = gen_server:call(?MODULE, {start_worker, QueueId}),
            ok
    end,
    add_to_queue(QueueId, Task).

open_queue(QueueId, {EtsId, HandlerPid}) ->
    true = ets:insert(wolfmq_queues, {QueueId, EtsId, HandlerPid}),
    ok.

close_queue(QueueId) ->
    true = ets:delete(wolfmq_queues, QueueId),
    ok.

%% gen_server callbacks
init(_Args) ->
    _ = ets:new(wolfmq_queues, [public, named_table, {read_concurrency, true}]),
    {ok, undefined}.

handle_call({start_worker, QueueId}, _From, State) ->
    Result = case is_existing_queue(QueueId) of
        true ->
            ok;
        false ->
            {ok, _Pid} = wolfmq_workers_sup:start_worker(QueueId),
            ok
    end,
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% internal
is_existing_queue(QueueId) ->
    case ets:lookup(wolfmq_queues, QueueId) of
        [] -> false;
        [{QueueId, _, HandlerPid}] -> is_process_alive(HandlerPid)
    end.

add_to_queue(QueueId, Tasks) when is_list(Tasks) ->
    [{QueueId, EtsId, _HandlerPid}] = ets:lookup(wolfmq_queues, QueueId),
    List = [{now(), Task} || Task <- Tasks],
    true = ets:insert(EtsId, List),
    ok;
add_to_queue(QueueId, Task) ->
    [{QueueId, EtsId, _HandlerPid}] = ets:lookup(wolfmq_queues, QueueId),
    Tuple = {now(), Task},
    true = ets:insert(EtsId, Tuple),
    ok.
