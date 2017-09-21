-module(wolfmq_mgr).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start_worker/1]).

%% gen_server callbacks
-export([init/1, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([code_change/3]).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_worker(QueueId) ->
    gen_server:call(?MODULE, {start_worker, QueueId}).

%% gen_server callbacks
init(_Args) ->
    {ok, undefined}.

handle_call({start_worker, QueueId}, _From, State) ->
    Result = case wolfmq_meta:is_existing_queue(QueueId) of
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
