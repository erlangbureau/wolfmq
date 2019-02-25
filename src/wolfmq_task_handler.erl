-module(wolfmq_task_handler).
-behaviour(wolfmq_handler).

%% wolfmq_handler
-export([handle_message/1]).

-define(ERR(Format, Args), error_logger:error_msg(Format, Args)).

%% wolfmq_handler
handle_message(Msg) ->
    case execute(Msg) of
        ok ->
            delete;
        {error, Err} ->
            ok = ?ERR("wolfmq error when processing ~p. ~p", [Msg, Err]),
            keep;
        {exception, Class, Reason, StackTrace}   ->
            ok = ?ERR("wolfmq exception when processing ~p. ~p:~p Stacktrace: ~p", [Msg, Class, Reason, StackTrace]),
            delete
    end.

%% internal
execute({Module, Fun, Args}) ->
    try erlang:apply(Module, Fun, Args) of
        ok -> ok;
        Err -> {error, Err}
    catch
        Class:Reason -> {exception, Class, Reason, erlang:get_stacktrace()}
    end;
execute({Fun, Args}) ->
    try erlang:apply(Fun, Args) of
        ok -> ok;
        Err -> {error, Err}
    catch
        Class:Reason -> {exception, Class, Reason, erlang:get_stacktrace()}
    end;
execute(Fun) when is_function(Fun) ->
    try Fun() of
        ok -> ok;
        Err -> {error, Err}
    catch
        Class:Reason -> {exception, Class, Reason, erlang:get_stacktrace()}
    end.
