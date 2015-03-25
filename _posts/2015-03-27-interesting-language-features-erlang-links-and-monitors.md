---
layout: post
title: Interesting Language Features - Erlang II
date: 2015-03-27T16:00:00Z-02:00
---

# Interesting Language Features - Erlang II

<quote class="disclaimer">This blog post is an article of a series which contains examples, explanations and details about interesting features of various programming languages. I have collected several examples of different characteristics, which definitely extended my view regarding programming, architecture and structure in general. I would love to hear your feedback about presented choices or description of yours favorite programming language feature.</quote>

### Introduction

In previous blog post, we talked about *pattern matching* and its usage in *functional programming* in general. But *Erlang* is more known in the programming community from something else. This purposely developed language, with support for distribution and concurrency baked in since beginning is mostly known as a solid foundation of long running systems, maintained for many years. Requirements imposed by telecommunication industry in 80s are still valid and actual for today's IT systems. We can benefit from whole platform, if we have to deal with strict requirements related with *reliability* and *fault-tolerance*.

*Erlang* unique features related with concurrency, reliability and distribution are based around very simple concepts - *isolation*, *lightweight processes* and powerful *`VM`* implementation. Back in a days, *Erlang* need for distribution was argued because of *hardware redundancy*. If you want to have *fault-tolerant* system, you need to provide redundant machines. Simply put - *two machines are handling errors much better than just one*.

But, if you have multiple concurrent processes, you need to treat errors differently - classical way of error handling or *defensive programming* techniques will not help us here. We need to deal with them in a different way. We have *supervisors* with their hierarchies and ability to *connect* and *observe* other processes. I would like to focus on second group.

<h3><i>Links and Monitors</i></h3>

*Isolation* is a very wise choice, when it comes to the reliability, because we can avoid *cascading failures*. But, how you will know that something failed actually? Besides *isolation* the very important thing is to have ability to *observe* other processes. By connecting two processes together via *link*, you are creating the bidirectional bond - process will be killed together with the other one that fails.

<section class="picture-section">
  <img class="half-sized-image" alt="Links in Erlang" src="http://learnyousomeerlang.com/static/img/link-exit.png" />
  <small>Image shamelessly taken from the amazing book <em>Learn You Some Erlang For Great Good!</em></small>
</section>

It sounds useful - you can group processes together via common concerns and bring them down together if one of them fails. Disable link is possible by both sides. But what if we would like to have more granular control on the exiting. We can either *monitor* a second process or *trap exits*. Lets look on the second method first:

{% highlight erlang %}
process_flag(trap_exit, true).
{% endhighlight %}

By doing that, process which traps exits will receive an additional message to the mailbox from the linked processes and itself. Only one type of error, invoked by the process itself, cannot be trapped - `exit(Pid, kill).`.

Besides that you can *observe* other process by setting unidirectional connection called a *monitor*. When the monitored process will go down, observer will receive a new message to the mailbox. As we said, it is a unidirectional relation, so it can be disabled only by the process that set up that connection earlier.

### Origin

All ideas looks like a very high level concepts, but it is actually the opposite:

<quote class="foreign">Links were invented by Mike Williams and based on the idea of a C-wire (a form of electrical circuit breaker).</quote>

Idea comes directly from the *“C-wire”* in early telephones. You should ground the *C-wire* to cancel the call. *Electronics* is always a very good place to collect a valuable inspiration regarding the system design. :wink:

### Credits

- [Learn You Some Erlang For Great Good! Chapter: Errors and Processes](http://learnyousomeerlang.com/errors-and-processes)
- [Erlang Mailing List, Origin of Links](http://erlang.org/pipermail/erlang-questions/2014-June/079885.html)
- [The Evolution of Erlang VM](http://www.erlang-factory.com/upload/presentations/247/erlang_vm_1.pdf)
