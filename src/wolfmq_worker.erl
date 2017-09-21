-module(wolfmq_worker).
-behaviour(gen_server).

%% API
-export([start_link/1, stop/1]).
-export([force_processing/1]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

-define(T, ?MODULE).
-record(state, {
    heartbeat_timer,
    idle_timer,
    heartbeat_timeout,
    idle_timeout,
    queue_id,
    ets_id
}).

%% API
start_link(Args) ->
    gen_server:start_link(?MODULE, [Args], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

force_processing(Pid) ->
    Pid ! process_queue,
    ok.

%% gen_server callbacks
init([QueueId]) ->
    {ok, HeartbeatTimeout}  = application:get_env(wolfmq, heartbeat_timeout),
    {ok, IdleTimeout}       = application:get_env(wolfmq, idle_timeout),
    EtsId = wolfmq_queue:create(),
    ok = wolfmq_meta:open_queue(QueueId, {EtsId, self()}),
    {ok, HeartbeatTimerRef}  = timer:send_after(0, process_queue),
    State = #state{
        heartbeat_timer     = HeartbeatTimerRef,
        heartbeat_timeout   = timer:seconds(HeartbeatTimeout),
        idle_timeout        = timer:seconds(IdleTimeout),
        queue_id            = QueueId,
        ets_id              = EtsId
    },
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(stop, #state{ets_id = EtsId, queue_id = QueueId} = State) ->
    ok = wolfmq_meta:close_queue(QueueId),
    ok = wolfmq_queue:map(EtsId, fun execute/1),
    ok = wolfmq_queue:destroy(EtsId),
    {stop, normal, State}.

handle_info(process_queue, #state{heartbeat_timer = HeartbeatTimerRef,
        idle_timer = IdleTimerRef1, idle_timeout = IdleTimeout,
        heartbeat_timeout = HeartbeatTimeout, ets_id = EtsId} = State) ->
    {ok, cancel} = timer:cancel(HeartbeatTimerRef),
    IdleTimerRef2 = case wolfmq_queue:is_empty(EtsId) of
        true ->
            start_idle_timer(IdleTimerRef1, IdleTimeout);
        false ->
            {ok, cancel} = cancel_idle_timer(IdleTimerRef1),
            ok = wolfmq_queue:map(EtsId, fun execute/1),
            undefined
    end,
    {ok, HeartbeatTimerRef2} = timer:send_after(HeartbeatTimeout, process_queue),
    Sate2 = State#state{
        heartbeat_timer = HeartbeatTimerRef2,
        idle_timer      = IdleTimerRef2
    },
    {noreply, Sate2};
handle_info(stop, #state{ets_id = EtsId, queue_id = QueueId} = State) ->
    ok = wolfmq_meta:close_queue(QueueId),
    ok = wolfmq_queue:map(EtsId, fun execute/1),
    ok = wolfmq_queue:destroy(EtsId),
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
    end;
execute(Fun) when is_function(Fun) ->
    try Fun() of
        ok -> ok;
        _ -> error
    catch
        _Class:_Reason -> exception
    end.
