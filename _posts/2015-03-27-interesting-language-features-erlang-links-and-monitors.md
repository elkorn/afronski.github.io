---
layout: post
title: Interesting Language Features - Erlang II
date: 2015-03-27T16:00:00Z-02:00
---

# Interesting Language Features - Erlang II

<quote class="disclaimer">This blog post is an article from a series which contains examples, explanations and details about interesting features of various programming languages. I have collected several examples of different characteristics, which definitely extended my view regarding programming, architecture and structure in general. I would love to hear your feedback about presented choices or description of yours favorite programming language feature.</quote>

### Introduction

In the previous blog post, we talked about *pattern matching* and its usage in *functional programming* in general. But *Erlang* is more known in the programming community for something different. This programming language, with support for distribution and concurrency baked in since its conception, is mostly known as a solid foundation for reliable, long running systems, maintained for many years. It was developed this way **deliberately** with all mentioned features in mind, because of requirements imposed by telecommunications industry in the 80s. Moreover, these requirements are still actual for today's IT systems. We can benefit from the whole platform, especially if we have to deal with strict requirements regarding *reliability* and *fault-tolerance*.

*Erlang's* unique features related to concurrency, reliability and distribution are based on very simple concepts - *isolation*, *lightweight processes* and a powerful *`VM`* implementation. Back in the day, *Erlang's* distribution needs were argued because of *hardware redundancy*. If you want a *fault-tolerant* system, you need at least two computers. You need to provide *redundancy*. Simply put - *two machines are able to handle multiple errors, one machine handles only the first error*. :wink:

But, if you have multiple concurrent processes, you need to treat errors differently - the classic way of error handling or *defensive programming* techniques will not help you here. *Erlang's* famous motto for that situations is **let it crash**. It does not mean, that we should crash the whole *VM* in case of error. We just need to deal with it in a different way and using different tools - *supervisors*, with their hierarchies and ability to *connect* and *observe* other processes. I would like to focus on the second group.

<h3><i>Links and Monitors</i></h3>

*Isolation* is a very wise choice when it comes to reliability, because we can avoid *cascading failures*. But, how will you know that something has actually failed? Besides *isolation*, the very important thing is to have the ability to *observe* other processes. By connecting two processes together via a *link*, you are creating a bidirectional bond - both processes will be killed in case one of them fails.

<section class="picture-section">
  <img class="half-sized-image" alt="Links in Erlang" src="http://learnyousomeerlang.com/static/img/link-exit.png" />
  <small>Image shamelessly taken from the amazing book <em>Learn You Some Erlang For Great Good!</em></small>
</section>

It sounds useful - you can group processes together with common concerns and bring them down together in case of errors. Disabling a link is possible on both sides. But what if we would like to have more granular control on the exiting flow. We can either *monitor* a second process or *trap exits*. Let's start with the second method:

{% highlight erlang %}
process_flag(trap_exit, true).
{% endhighlight %}

By doing that, the process which *trap exits* will receive an additional message to the mailbox if the linked processes will exit abruptly with an erroneous reason. Also, if the process itself will exit with an error - an exit signal will be trapped. Only one type of error, called a *`kill`* and invoked by the process itself, cannot be trapped - you can do it by `exit(Pid, kill).`.

Besides that you can *observe* other processes by setting a unidirectional connection called a *monitor*. When the monitored process will go down, observer will receive a new message directly to the mailbox. As we said, it is a unidirectional relation, so it can be disabled only by the process that set up that connection earlier.

### Origin

All of the described ideas look like very high level concepts, but the opposite is actually true:

<quote class="foreign">Links were invented by Mike Williams and based on the idea of a C-wire (a form of electrical circuit breaker).</quote>

The idea comes directly from the *“C-wire”* in early telephones. In order to cancel a problematic call, you should ground the *C-wire*. *Electronics* is always a very good place to collect valuable inspiration regarding *system design* and *fault-tolerance*. :wink:

### Credits

- [Learn You Some Erlang For Great Good! Chapter: Errors and Processes](http://learnyousomeerlang.com/errors-and-processes)
- [Erlang Mailing List, Origin of Links](http://erlang.org/pipermail/erlang-questions/2014-June/079885.html)
- [The Evolution of Erlang VM](http://www.erlang-factory.com/upload/presentations/247/erlang_vm_1.pdf)
