WolfMQ 
============

WolfMQ is a small and fast Erlang message broker and queued task processor

Description
===========

**Wolfmq features:**
* Automatic creation and closing of the queues
* Use any term as a queue id
* Run different queues on different supervisors
* By default, the queue is a task processor, but you can change behaviour



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

