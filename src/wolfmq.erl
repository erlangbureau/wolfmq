-module(wolfmq).

%% API
-export([start/0, stop/0]).
-export([push/1, push/2]).

%% API
push(Task) ->
    QueueId = self(),
    push(QueueId, Task).

push(QueueId, Task) ->
    wolfmq_mgr:push(QueueId, Task).
