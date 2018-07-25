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
    external_queue_id,
    internal_queue_id,
    handler
}).

%% API
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop(Pid) ->
    gen_server:stop(Pid).

force_processing(Pid) ->
    Pid ! process_queue,
    ok.

%% gen_server callbacks
init([ExternalQueueId, Opts]) ->
    {ok, HeartbeatTimeout}  = application:get_env(wolfmq, heartbeat_timeout),
    {ok, IdleTimeout}       = application:get_env(wolfmq, idle_timeout),
    InternalQueueId         = wolfmq_queue:create(),
    ok = wolfmq_queues_catalog:insert(ExternalQueueId, {InternalQueueId, self()}),
    {ok, HeartbeatTimerRef} = timer:send_after(0, process_queue),
    State = #state{
        heartbeat_timer     = HeartbeatTimerRef,
        heartbeat_timeout   = timer:seconds(HeartbeatTimeout),
        idle_timeout        = timer:seconds(IdleTimeout),
        external_queue_id   = ExternalQueueId,
        internal_queue_id   = InternalQueueId,
        handler             = maps:get(handler, Opts, wolfmq_task_handler)
    },
    {ok, State}.

terminate(_Reason, State) ->
    ExternalQueueId = State#state.external_queue_id,
    InternalQueueId = State#state.internal_queue_id,
    Handler         = State#state.handler,
    ok = wolfmq_queues_catalog:delete(ExternalQueueId),
    HandleFun = fun Handler:handle_message/1,
    ok = wolfmq_queue:map(InternalQueueId, HandleFun),
    ok = wolfmq_queue:destroy(InternalQueueId),
    ok.

handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, ignore, State}.

handle_info(process_queue, State) ->
    HeartbeatTimerRef   = State#state.heartbeat_timer,
    IdleTimerRef1       = State#state.idle_timer,
    IdleTimeout         = State#state.idle_timeout,
    HeartbeatTimeout    = State#state.heartbeat_timeout,
    InternalQueueId     = State#state.internal_queue_id,
    Handler             = State#state.handler,
    {ok, cancel} = timer:cancel(HeartbeatTimerRef),
    IdleTimerRef2 = case wolfmq_queue:is_empty(InternalQueueId) of
        true ->
            start_idle_timer(IdleTimerRef1, IdleTimeout);
        false ->
            {ok, cancel} = cancel_idle_timer(IdleTimerRef1),
            HandleFun = fun Handler:handle_message/1,
            ok = wolfmq_queue:map(InternalQueueId, HandleFun),
            undefined
    end,
    {ok, HeartbeatTimerRef2} = timer:send_after(HeartbeatTimeout, process_queue),
    Sate2 = State#state{
        heartbeat_timer = HeartbeatTimerRef2,
        idle_timer      = IdleTimerRef2
    },
    {noreply, Sate2};
handle_info(stop, State) ->
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
