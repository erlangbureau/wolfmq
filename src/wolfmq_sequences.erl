-module(wolfmq_sequences).

%% API
-export([init/0]).
-export([create/1]).
-export([next/1]).
-export([delete/1]).
-export([is_existing/1]).

%% API
init() ->
    _ = ets:new(?MODULE, [public, named_table, {read_concurrency, true}]),
    ok.

create(SeqId) ->
    _ = ets:insert(?MODULE, {SeqId, 0}),
    ok.

next(SeqId) ->
    ets:update_counter(?MODULE, SeqId, {2, 1}).

delete(SeqId) ->
    _ = ets:delete(?MODULE, SeqId),
    ok.

is_existing(SeqId) ->
    ets:member(?MODULE, SeqId).
