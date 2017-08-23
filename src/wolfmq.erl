-module(wolfmq).

%% API
-export([push/2]).

%% API
push(QueueId, Task) ->
    wolfmq_mgr:push(QueueId, Task).
