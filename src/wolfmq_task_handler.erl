-module(wolfmq_task_handler).
-behaviour(wolfmq_handler).

%% wolfmq_handler
-export([handle_message/1]).

%% wolfmq_handler
handle_message(Msg) ->
    case execute(Msg) of
        ok          -> ok;
        error       -> keep;
        exception   -> ok
    end.

%% internal
execute({Module, Fun, Args}) ->
    try erlang:apply(Module, Fun, Args) of
        ok -> ok;
        _ -> error
    catch
        _Class:_Reason -> exception
    end;
execute({Fun, Args}) ->
    try erlang:apply(Fun, Args) of
        ok -> ok;
        _ -> error
    catch
        _Class:_Reason -> exception
    end;
execute(Fun) when is_function(Fun) ->
    try Fun() of
        ok -> ok;
        _ -> error
    catch
        _Class:_Reason -> exception
    end.
