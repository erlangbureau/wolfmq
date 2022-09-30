-module(wolfmq_task_handler).
-behaviour(wolfmq_handler).

%% wolfmq_handler
-export([handle_message/1]).

-define(ERR(Format, Args), error_logger:error_msg(Format, Args)).

%% wolfmq_handler
handle_message(Msg) ->
    handle_message(Msg, 0).

%% internal
handle_message(Msg, Attempt) when Attempt < 11 ->
    case execute(Msg) of
        ok ->
            delete;
        {error, Err} ->
            ok = ?ERR("wolfmq error when processing ~p. ~p on attempt #~p", [Msg, Err, Attempt]),
            ok = timer:sleep(10 * power(2, Attempt)),
            handle_message(Msg, Attempt + 1);
        {exception, Class, Reason, StackTrace}   ->
            ok = ?ERR("wolfmq exception when processing ~p. ~p:~p Stacktrace: ~p", [Msg, Class, Reason, StackTrace]),
            delete
    end;
handle_message(_Msg, 11) ->
    keep.

power(2, 0) -> 1;
power(2, 1) -> 2;
power(2, 2) -> 4;
power(2, 3) -> 8;
power(2, 4) -> 16;
power(2, 5) -> 32;
power(2, 6) -> 64;
power(2, 7) -> 128;
power(2, 8) -> 256;
power(2, 9) -> 512;
power(2, 10) -> 1024.

execute({Module, Fun, Args}) ->
    try erlang:apply(Module, Fun, Args) of
        ok -> ok;
        Err -> {error, Err}
    catch
        Class:Reason:Stacktrace -> {exception, Class, Reason, Stacktrace}
    end;
execute({Fun, Args}) ->
    try erlang:apply(Fun, Args) of
        ok -> ok;
        Err -> {error, Err}
    catch
        Class:Reason:Stacktrace -> {exception, Class, Reason, Stacktrace}
    end;
execute(Fun) when is_function(Fun) ->
    try Fun() of
        ok -> ok;
        Err -> {error, Err}
    catch
        Class:Reason:Stacktrace -> {exception, Class, Reason, Stacktrace}
    end.
