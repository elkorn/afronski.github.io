---
layout: post
title: Interesting Language Features - Erlang IV
date: 2015-06-11T16:00+0200
categories:
  - interesting-language-features
tags:
  - series
  - programming-languages
  - erlang
  - erlang-behaviours
---

# Interesting Language Features - Erlang IV

<quote class="disclaimer">This blog post is a next article from a series which contains examples, explanations and details about interesting features of various programming languages. I have collected several examples of different characteristics, which definitely extended my view regarding programming, architecture and structure in general. I would love to hear your feedback about presented choices or description of yours favorite programming language feature.</quote>

### Introduction

In the [previous blog post](http://www.afronski.pl/2015/05/14/interesting-language-features-erlang-application-behavior.html) we talked about *behaviors*, in particular about *application* behavior. It is a primary way of reusing common abstractions.

It may look limiting at the first sight that we have only couple of them available by default. But fortunately, we are not limited only to the abstractions prepared by the *OTP* team. *We can easily create new ones* - and many *Erlang* teams willingly takes that opportunity. In this blog post I would like to show you various abstractions hidden inside RabbitMQ.

### What is *RabbitMQ*?

<img class="right rabbitmq-logo" alt="RabbitMQ Logo" src="/assets/RabbitMQ.png" />

It is a robust, easy to use message broker ready to use with your applications - it can be a *communication backbone* for your system. It supports multiple protocols by default - *AMQP*, *MQTT* and *STOMP*. Also, it has various client libraries for many programming languages - you can integrate with it from almost any kind of environment. The main idea behind it is pretty simple - it accepts and forwards messages. You can think about it as an any abstraction related with letters and messaging e.g. post office - when you send mail to the post box you are pretty sure that postman will eventually deliver it to your recipient. Using this metaphor *RabbitMQ* is a post box, a *post office* and a *postman* in one thing. If you want to get familiar with this tool, you should start with [this article](https://www.rabbitmq.com/getstarted.html).

*RabbitMQ* is written in *Erlang* and it is a base for its reliability and concurrency features. Also, thanks to built-in distribution it is much easier to implement custom mechanisms, even if they are using different thing than standard *Erlang* distribution to handle that problem. As you probably know, in *Erlang*, *behaviors* are the main mechanism for providing reusable abstractions - we can easily use that ones prepared by an *OTP* team like `supervisor`, `application` or `gen_server`.

But, as we said in the introduction, we are not limited only to the prepared ones. *We can create our own behaviors*.

### How we can create a *custom behavior*?

To create your own behavior, you must make a module that exports a function `behaviour_info/1`. Note, that while Erlang is *American-English-friendly* in regards to declaring behaviors, it is not as friendly when you are defining them. For example, you can use either `-behavior(gen_server).` or `-behaviour(gen_server).` when you declare you module implements the `gen_server` behavior, but if you name the `behaviour_info/1` function without the *'u'* it will not work.

`behaviour_info/1` just needs to return the list of required exports of an implementation. Here's an example:

{% highlight erlang linenos %}
-module(gen_foo).
-export([ behaviour_info/1 ]).

behaviour_info(callbacks) ->
  [ {foo, 0}, {bar, 1}, {baz, 2} ];

behavior_info(_) ->
    undefined.
{% endhighlight %}

This declares three callbacks for the `gen_foo` behavior: `foo/0`, `bar/1`, and `baz/2`. Function simply returns an array of *two elements tuples* with the function name andan arity.

When a module declares that it implements `gen_foo` now, the Erlang compiler will check to make sure it exports and implements the required callbacks, and it will print warnings if this is not the case:

{% highlight erlang linenos %}
-module(fooer).
-behavior(gen_foo).

-export([ foo/0, bar/1, baz/2 ]).

foo() -> foo.
bar(a) -> {bar, a}.
baz(a, b) -> {baz, a, b}.
{% endhighlight %}

You can also use tools like *Dialyzer* and *Typer* - with prepared type specifications, you can easily verify that the new behavior is used properly (according to the specification) and that there are no type errors which can be caught due to *static code analysis*.

### Custom behaviors available inside *RabbitMQ*

In RabbitMQ we have implemented several routing algorithms in the form of *exchanges* (let's call them the *gateways* by which messages are coming to the system). We can say that each exchange has a specific type. By default *RabbitMQ* has four exchanges: *direct*, *fanout*, *topic* and *headers*. But also, it allows the user to add new exchange types via plugins.

User, which will provide new exchange type needs to implement the `rabbit_exchange_type` behaviour. Similar pattern can be applied to the `rabbitmq_backing_queue` which is responsible for various techniques related with backing stores and e.g. persistent queues. Aforementioned abstractions have pretty simple implementation - most of the module code is related with type specifications used for the verification. More complicated behaviors are mirroring and enhancing the standard ones - inside *RabbitMQ* maintainers implemented `supervisor2` and `gen_server2` - code is really long and complex, because those behaviors need to mirror the original implementations, and also should have additional features.

If you have already some experience with *Erlang* I recommend you to dive into the implementation of two mentioned behaviors (and look at the originals as well - comments are really helpful). And by that, I would like to finish that blog post - in the next one, we will switch the language - now it is time to look into a different place to search for an interesting feature. Stay tuned! :wink: 

### Credits

- [Dissecting the rabbit: RabbitMQ Internal Architecture](http://www.slideshare.net/old_sound/dissecting-the-rabbit)
- [Alvaro Videla's blog](http://videlalvaro.github.io) - amazing source of knowledge about RabbitMQ internals.
- [rabbitmq_backing_queue](https://github.com/rabbitmq/rabbitmq-server/blob/master/src/rabbit_backing_queue.erl)
- [rabbitmq_exchange_type](https://github.com/rabbitmq/rabbitmq-server/blob/master/src/rabbit_exchange_type.erl)
- [gen_server2](https://github.com/rabbitmq/rabbitmq-server/blob/master/src/gen_server2.erl)
- [supervisor2](https://github.com/rabbitmq/rabbitmq-server/blob/master/src/supervisor2.erl)
