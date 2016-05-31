WolfMQ 
============

[![Join the chat at https://gitter.im/erlangbureau/wolfmq](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/erlangbureau/wolfmq?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

WolfMQ is a small and fast Erlang message broker.

Getting Started
===============

```erl

%% Push
1> QueueId = 1.
1
2> ok = wolfmq:push(QueueId, {io, format, ["Some text~n", []]}).
ok

```

Project Chat Room
=================
[![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/erlangbureau/wolfmq?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

