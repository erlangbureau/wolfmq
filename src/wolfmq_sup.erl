-module(wolfmq_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% API
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor callbacks
init([]) ->
    WorkersSup = {
        wolfmq_workers_sup,
        {wolfmq_workers_sup, start_link, []},
		transient, 5000, supervisor, [wolfmq_workers_sup]
    },
    QueueMgr = {
        wolfmq_mgr,
        {wolfmq_mgr, start_link, []},
		permanent, 100, worker, [wolfmq_mgr]
    },
    Procs = [WorkersSup, QueueMgr],
    {ok, { {one_for_one, 5, 10}, Procs} }.
