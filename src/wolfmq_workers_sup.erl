-module(wolfmq_workers_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_worker/1]).

%% supervisor callbacks
-export([init/1]).

%% API
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker(QueueId) ->
    supervisor:start_child(?MODULE, [QueueId]).

%% supervisor callbacks
init([]) ->
    Worker = {
        wolfmq_worker,
        {wolfmq_worker, start_link, []},
        transient, brutal_kill, worker, [wolfmq_worker]
    },
    {ok, {{simple_one_for_one, 250, 5}, [Worker]}}.
