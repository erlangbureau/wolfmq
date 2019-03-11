-module(wolfmq_worker).
-behaviour(gen_server).

%% API
-export([start_link/1, stop/1]).
-export([force_processing/1]).
-export([get_queue_id/0]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

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

get_queue_id() ->
    erlang:get(external_queue_id).

%% gen_server callbacks
init([ExternalQueueId, Opts]) ->
    HeartbeatTimeout    = get_timeout_env(heartbeat_timeout, 1),
    IdleTimeout         = get_timeout_env(idle_timeout, 10),
    InternalQueueId     = wolfmq_queue:create(),
    ok = wolfmq_queues_catalog:insert(ExternalQueueId, {InternalQueueId, self()}),
    ok = save_queue_id(ExternalQueueId),
    HeartbeatTimerRef = start_heartbeat_timer(0),
    State = #state{
        heartbeat_timer     = HeartbeatTimerRef,
        heartbeat_timeout   = HeartbeatTimeout,
        idle_timeout        = IdleTimeout,
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
    _ = stop_heartbeat_timer(HeartbeatTimerRef),
    IdleTimerRef2 = case wolfmq_queue:is_empty(InternalQueueId) of
        true ->
            start_idle_timer(IdleTimerRef1, IdleTimeout);
        false ->
            _ = cancel_idle_timer(IdleTimerRef1),
            HandleFun = fun Handler:handle_message/1,
            ok = wolfmq_queue:map(InternalQueueId, HandleFun),
            undefined
    end,
    HeartbeatTimerRef2 = start_heartbeat_timer(HeartbeatTimeout),
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
save_queue_id(ExternalQueueId) ->
    _ = erlang:put(external_queue_id, ExternalQueueId),
    ok.

get_timeout_env(Name, Default) ->
    Timeout = application:get_env(wolfmq, Name, Default),
    timer:seconds(Timeout).

start_heartbeat_timer(Timeout) ->
    erlang:send_after(Timeout, self(), process_queue).

stop_heartbeat_timer(TimerRef) ->
    erlang:cancel_timer(TimerRef).

start_idle_timer(undefined, Timeout) ->
    erlang:send_after(Timeout, self(), stop);
start_idle_timer(OldTimer, _Timeout) ->
    OldTimer.

cancel_idle_timer(undefined) ->
    false;
cancel_idle_timer(TimerRef) ->
    erlang:cancel_timer(TimerRef).
