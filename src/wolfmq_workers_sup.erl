-module(wolfmq_workers_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([start_worker/3]).

%% supervisor callbacks
-export([init/1]).

%% API
start_link(GroupId) ->
    Name = group_id_to_process_name(GroupId),
    supervisor:start_link({local, Name}, ?MODULE, []).

start_worker(GroupId, QueueId, Opts) ->
    Name = group_id_to_process_name(GroupId),
    supervisor:start_child(Name, [[QueueId, Opts]]).

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

%% internal
group_id_to_process_name(GroupId) ->
    Id = atom_to_binary(GroupId, utf8),
    Name = <<"wolfmq_group_sup_", Id/binary>>,
    binary_to_atom(Name, utf8).
