-module(wolfmq_queue).

%% API
-export([create/0, destroy/1]).
-export([enqueue/2, dequeue/1]).
-export([map/2]).
-export([is_empty/1]).

%% API
create() ->
    ets:new(?MODULE, [public, ordered_set, {write_concurrency, true}]).

destroy(EtsId) ->
    true = ets:delete(EtsId),
    ok.

enqueue(EtsId, Item) ->
    true = ets:insert(EtsId, Item).

dequeue(EtsId) ->
    Key  = ets:first(EtsId),
    Item = ets:lookup(EtsId, Key),
    true = ets:delete(EtsId, Key),
    Item.

map(EtsId, Predicate) ->
    map(EtsId, Predicate, ets:first(EtsId)).

map(_EtsId, _Predicate, '$end_of_table') ->
    ok;
map(EtsId, Predicate, Key) ->
    _ = case ets:lookup(EtsId, Key) of
        [{_Key, Item}] ->
            case Predicate(Item) of
                ok        -> ets:delete(EtsId, Key);
                error     -> ok;
                exception -> ets:delete(EtsId, Key) %% because queue overflow
            end;
        _ ->
            ets:delete(EtsId, Key)
    end,
    map(EtsId, Predicate, ets:first(EtsId)).

is_empty(EtsId) ->
    ets:info(EtsId, size) =:= 0.
