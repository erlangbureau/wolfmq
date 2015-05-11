-module(wolfmq).

%% API
-export([start/0, stop/0]).
-export([push/1, push/2]).

%% API
start() ->
    start(?MODULE).

stop() ->
    application:stop(?MODULE).

push(Task) ->
    QueueId = self(),
    push(QueueId, Task).

push(QueueId, Task) ->
    wolfmq_mgr:push(QueueId, Task).


%% internal
start(AppName) ->
    F = fun({App, _, _}) -> App end,
    RunningApps = lists:map(F, application:which_applications()),
    ok = load(AppName),
    {ok, Dependencies} = application:get_key(AppName, applications),
    [begin
        ok = start(A)
    end || A <- Dependencies, not lists:member(A, RunningApps)],
    ok = application:start(AppName).

load(AppName) ->
    F = fun({App, _, _}) -> App end,
    LoadedApps = lists:map(F, application:loaded_applications()),
    case lists:member(AppName, LoadedApps) of
        true ->
            ok;
        false ->
            ok = application:load(AppName)
    end.
