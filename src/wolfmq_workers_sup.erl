-module(wolfmq_workers_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([start_worker/3]).

%% supervisor callbacks
-export([init/1]).

%% API
start_link(GroupId) ->
    supervisor:start_link({local, GroupId}, ?MODULE, []).

start_worker(GroupId, QueueId, Opts) ->
    supervisor:start_child(GroupId, [[QueueId, Opts]]).

%% supervisor callbacks
init([]) ->
    Worker = #{
        id          => wolfmq_worker,
        start       => {wolfmq_worker, start_link, []},
        restart     => transient,
        shutdown    => brutal_kill,
        type        => worker,
        modules     => [wolfmq_worker]
    },
    {ok, {{simple_one_for_one, 250, 5}, [Worker]}}.
