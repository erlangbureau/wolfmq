-module(wolfmq_mgr).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start_worker/2]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_worker(QueueId, Opts) ->
    gen_server:call(?MODULE, {start_worker, QueueId, Opts}).

%% gen_server callbacks
init(_Args) ->
    {ok, undefined}.

terminate(_Reason, _State) ->
    ok.

handle_call({start_worker, {GroupId, _QId} = QueueId, Opts}, _From, State) ->
    _ = case wolfmq_queues_catalog:is_existing(QueueId) of
        true ->
            ok;
        false ->
            _ = wolfmq_sup:start_group(GroupId),
            {ok, _Pid} = wolfmq_workers_sup:start_worker(GroupId, QueueId, Opts)
    end,
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ignore, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
