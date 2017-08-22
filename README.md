WolfMQ 
============

WolfMQ is a small and fast Erlang message broker and queued task processor

Getting Started
===============

```erl

%% Push
1> QueueId = 1.
1
2> F = fun() -> io:format("F message") end.
#Fun<erl_eval.20.80484245>
3> MFA = {io, format, ["MFA message~n", []]}.
{io,format,["MFA message~n",[]]}
4> ok = wolfmq:push(QueueId, F).
ok
4> ok = wolfmq:push(QueueId, MFA).
ok

```

Project Chat Room
=================
[![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/erlangbureau/wolfmq)

