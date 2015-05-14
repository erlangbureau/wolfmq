-module(wolfmq_worker).
-behaviour(gen_server).

%% API
-export([start_link/1, stop/1]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

-define(T, ?MODULE).
-record(state, {
    activity_timer,
    idle_timer,
    idle_timeout,
    queue_id,
    ets_id}).

%% API
start_link(Args) ->
    gen_server:start_link(?MODULE, [Args], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

%% gen_server callbacks
init([QueueId]) ->
    Secs = application:get_env(hts, queue_idle_timeout, 10),
    IdleTimeout = timer:seconds(Secs),
    HandlerPid = self(),
    EtsId = ets:new(?T, [public, ordered_set, {write_concurrency, true}]),
    ok = wolfmq_mgr:open_queue(QueueId, {EtsId, HandlerPid}),
    {ok, ActivityTimerRef} = timer:send_after(1000, process_queue),
    {ok, #state{activity_timer = ActivityTimerRef, 
                idle_timeout = IdleTimeout,
                queue_id = QueueId,
                ets_id = EtsId}}.

terminate(_Reason, _State) ->
    ok.

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(stop, #state{ets_id = EtsId, queue_id = QueueId} = State) ->
    ok = wolfmq_mgr:close_queue(QueueId),
    ok = process_queue(EtsId),
    true = ets:delete(EtsId),
    {stop, normal, State}.
                                                                
handle_info(process_queue, #state{activity_timer = ActivityTimerRef, 
        idle_timer = IdleTimerRef1, idle_timeout = IdleTimeout, 
        ets_id = EtsId} = State) ->
    {ok, cancel} = timer:cancel(ActivityTimerRef),
    EtsInfoList = ets:info(EtsId),
    IdleTimerRef2 = case proplists:get_value(size, EtsInfoList) of
        Size when Size > 0 ->
            {ok, cancel} = cancel_idle_timer(IdleTimerRef1),
            ok = process_queue(EtsId),
            undefined;
        _ ->
            start_idle_timer(IdleTimerRef1, IdleTimeout)
    end,
    {ok, ActivityTimerRef2} = timer:send_after(1000, process_queue),
    Sate2 = State#state{activity_timer = ActivityTimerRef2, 
        idle_timer = IdleTimerRef2},
    {noreply, Sate2};
handle_info(stop, #state{ets_id = EtsId, queue_id = QueueId} = State) ->
    ok = wolfmq_mgr:close_queue(QueueId),
    ok = process_queue(EtsId),
    true = ets:delete(EtsId),
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal
start_idle_timer(undefined, Timeout) ->
    {ok, IdleTimerRef2} = timer:send_after(Timeout, stop),
    IdleTimerRef2;
start_idle_timer(OldTimer, _Timeout) ->
    OldTimer.

cancel_idle_timer(undefined) ->
    {ok, cancel};
cancel_idle_timer(TimerRef) ->
    timer:cancel(TimerRef).

process_queue(Table) ->
    process_queue(Table, ets:first(Table)).

process_queue(_Table, '$end_of_table') ->
    ok;
process_queue(Table, Key) ->
    _ = case ets:lookup(Table, Key) of
        [{Key, Msg}] ->
            case execute(Msg) of
                ok        -> ets:delete(Table, Key);
                error     -> ok;
                exception -> ets:delete(Table, Key)
            end;
        _ ->
            ets:delete(Table, Key)
    end,
    process_queue(Table, ets:first(Table)).

execute({Module, Fun, Args}) ->
    try erlang:apply(Module, Fun, Args) of
        ok -> ok;
        _ -> error
    catch
        _Class:_Reason -> exception
    end;
execute({Fun, Args}) ->
    try erlang:apply(Fun, Args) of
        ok -> ok;
        _ -> error
    catch
        _Class:_Reason -> exception
    end.
