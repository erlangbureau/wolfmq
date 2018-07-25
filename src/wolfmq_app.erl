-module(wolfmq_app).
-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%% application callbacks
start(_StartType, _StartArgs) ->
    ok = wolfmq_queues_catalog:init(),
    ok = wolfmq_sequences:init(),
    wolfmq_sup:start_link().

stop(_State) ->
    ok.
