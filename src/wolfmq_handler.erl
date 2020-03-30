-module(wolfmq_handler).

-callback handle_message(Msg) -> Result
when
    Msg     :: term(),
    Result  :: keep | delete.
