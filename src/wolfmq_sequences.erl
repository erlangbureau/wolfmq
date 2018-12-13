-module(wolfmq_sequences).

%% API
-export([init/0]).
-export([next/1]).
-export([delete/1]).

%% API
init() ->
    _ = ets:new(?MODULE, [public, named_table, {read_concurrency, true}]),
    ok.

next(SeqId) ->
    ets:update_counter(?MODULE, SeqId, {2, 1}, {SeqId, 0}).

delete(SeqId) ->
    _ = ets:delete(?MODULE, SeqId),
    ok.
