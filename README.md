WolfMQ 
============

WolfMQ is a small and fast Erlang message broker and queued task processor

Getting Started
===============

```erl
1> application:start(wolfmq).
ok
2> 
2> QueueId = erlang:unique_integer([monotonic, positive]) rem 10.
1
3>
3> ok = wolfmq:push(QueueId, fun() -> io:format("Hello world!~n"), ok end).
Hello world!
ok
4> 
4> ok = wolfmq:push(QueueId, {io, format, ["Hello world!~n"]}).                
Hello world!
ok
```

Project Chat Room
=================
[![Gitter chat](https://badges.gitter.im/gitterHQ/gitter.png)](https://gitter.im/erlangbureau/wolfmq)

