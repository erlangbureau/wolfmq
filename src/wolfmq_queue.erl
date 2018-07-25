-module(wolfmq_queue).

%% API
-export([create/0, destroy/1]).
-export([enqueue/2, dequeue/1]).
-export([map/2]).
-export([is_empty/1]).
-export([size/1]).

-compile({no_auto_import, [size/1]}).

%% API
create() ->
    ets:new(?MODULE, [public, ordered_set, {write_concurrency, true}]).

destroy(QueueId) ->
    true = ets:delete(QueueId),
    ok.

enqueue(QueueId, Item) ->
    true = ets:insert(QueueId, Item).

dequeue(QueueId) ->
    Key  = ets:first(QueueId),
    Item = ets:lookup(QueueId, Key),
    true = ets:delete(QueueId, Key),
    Item.

map(QueueId, Predicate) ->
    map(QueueId, Predicate, ets:first(QueueId)).

map(_QueueId, _Predicate, '$end_of_table') ->
    ok;
map(QueueId, Predicate, Key) ->
    _ = case ets:lookup(QueueId, Key) of
        [{_Key, Item}] ->
            case Predicate(Item) of
                ok      -> ets:delete(QueueId, Key);
                keep    -> ok
            end;
        _ ->
            ets:delete(QueueId, Key)
    end,
    map(QueueId, Predicate, ets:first(QueueId)).

is_empty(QueueId) ->
    size(QueueId) =:= 0.

size(QueueId) ->
    case ets:info(QueueId, size) of
        undefined -> 0;
        N -> N
    end.
