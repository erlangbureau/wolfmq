-module(wolfmq_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_group/1]).

%% supervisor callbacks
-export([init/1]).

%% API
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_group(GroupId) ->
    WorkersSup = #{
        id          => GroupId,
        start       => {wolfmq_workers_sup, start_link, [GroupId]},
        restart     => transient,
        shutdown    => infinity,
        type        => supervisor,
        modules     => [wolfmq_workers_sup]
    },
    supervisor:start_child(?MODULE, WorkersSup).

%% supervisor callbacks
init([]) ->
    QueueMgr = #{
        id          => wolfmq_mgr,
        start       => {wolfmq_mgr, start_link, []},
        restart     => permanent,
        shutdown    => 1000,
        type        => worker,
        modules     => [wolfmq_mgr]
    },
    {ok, {{one_for_one, 100, 1}, [QueueMgr]}}.
