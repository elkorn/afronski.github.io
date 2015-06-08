---
layout: post
title: Interesting Language Features - Erlang IV
date: 2015-06-11T16:00+0200
---

# Interesting Language Features - Erlang IV

<quote class="disclaimer">This blog post is a next article from a series which contains examples, explanations and details about interesting features of various programming languages. I have collected several examples of different characteristics, which definitely extended my view regarding programming, architecture and structure in general. I would love to hear your feedback about presented choices or description of yours favorite programming language feature.</quote>

### Introduction

In the [previous blog post](http://www.afronski.pl/2015/05/14/interesting-language-features-erlang-application-behavior.html) we talked about *behaviors*, in particular about *application* behavior. It is a primary way of reusing common abstractions.

It may look limiting at the first sight that we have only couple of them available by default. But fortunately, we are not limited only to the abstractions prepared by the *OTP* team. *We can easily create new ones* - and many *Erlang* teams willingly takes the opportunity. In this blog post I would like to show various abstractions hidden inside RabbitMQ.

### What is *RabbitMQ*?

<img class="right rabbitmq-logo" alt="RabbitMQ Logo" src="/assets/RabbitMQ.png" />

### How we can create a *custom behavior*?

### Custom behaviors available inside *RabbitMQ*

*TODO*: http://videlalvaro.github.io/2013/09/rabbitmq-internals-validating-erlang-behaviours.html

### Credits

- [Dissecting the rabbit: RabbitMQ Internal Architecture](http://www.slideshare.net/old_sound/dissecting-the-rabbit)
- [Alvaro Videla's blog](http://videlalvaro.github.io) - amazing source of knowledge about RabbitMQ internals.
- [rabbitmq_backing_queue](https://github.com/rabbitmq/rabbitmq-server/blob/master/src/rabbit_backing_queue.erl)
- [rabbitmq_exchange_type](https://github.com/rabbitmq/rabbitmq-server/blob/master/src/rabbit_exchange_type.erl)
- [gen_server2](https://github.com/rabbitmq/rabbitmq-server/blob/master/src/gen_server2.erl)
- [supervisor2](https://github.com/rabbitmq/rabbitmq-server/blob/master/src/supervisor2.erl)
