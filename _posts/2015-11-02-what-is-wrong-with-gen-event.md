---
layout: post
title: What is wrong with gen_event?
date: 2015-11-02T16:00+0100
tags:
  - erlang
  - erlang-behaviours
  - patterns
  - programming-languages
  - gen_event
---

# What is wrong with `gen_event`?

<quote class="citation">I never used gen_event, I think it is a bad pattern.</quote>

At first it may look like a controversial statement, but I heard a lot of those complaints from other people. Originally, I heard that exact statement during the presentation [made by Garrett Smith about pattern language](https://www.youtube.com/watch?v=UUvU8cjCIcs) - someone asked about that behavior at the end. More recently I heard similar thing in [José Valim's presentation about what will come next in Elixir](https://www.youtube.com/watch?v=9RB1JCKe3GY).

It confuses me every time I hear that, so I want to investigate topic more deeply. But before we will dive into reasons and explanations, let's recall what is the purpose of this behavior.

## What is `gen_event`?

OTP introduces two different terms regarding that behavior - an *event manager* and *event handler* modules.

Responsibility of *event manager* is being a named object which can receive events. An *event* can be, for example: an error, an alarm, or some information that is to be logged. Inside manager we can have 0, 1 or more *event handlers* installed. Responsibility of the handler is to process an *event*.

When the *event manager* is notified about an event, it will be processed by all installed handlers. The easiest way to imagine that is to think about manager as a sink for incoming messages and handlers as different implementations which are writing messages to disk, database or terminal.

Another example can be taken from my implementation of Francesco Cesarini's assignment called [Wolves, Rabbits and Carrots simulation](https://github.com/afronski/wolves-and-rabbits-world-simulation). Main purpose of that task is to introduce concurrency, but internally it is a simulation - so certain events are happening, and they will be broadcasted to the rest of entities.

In that case `simulation_event_stream` is an *event manager*:

{% highlight erlang linenos %}
-module(simulation_event_stream).

-export([ start_link/0,
          component_ready/1,
          notify/3, notify/4,
          attach_handler/1,
          remove_handler/1 ]).

start_link() ->
    {ok, Pid} = gen_event:start_link({local, ?MODULE}),

    gen_event:add_handler(?MODULE, simulation_cli_handler, []),
    component_ready(?MODULE),

    {ok, Pid}.

component_ready(Name) ->
    gen_event:notify(?MODULE, {Name, ready}).

notify(Name, Action, State) ->
    gen_event:notify(?MODULE, {Name, Action, State}).

notify(Name, Pid, Action, State) ->
    gen_event:notify(?MODULE, {Name, Pid, Action, State}).

attach_handler(Handler) ->
    gen_event:add_handler(?MODULE, Handler, []).

remove_handler(Handler) ->
    gen_event:delete_handler(?MODULE, Handler, []).
{% endhighlight %}

We can easily add and remove *event handlers*. The *event manager* essentially maintains a list of `{Module, State}` pairs, where each `Module` is an event handler, and `State` is the internal state of that event handler.

One of the *handlers* implementation - `simulation_cli_handler` - is related with writing messages to the console. It is the actual `gen_event` callback module, so all handlers are implementations of that abstraction:

{% highlight erlang linenos %}
-module(simulation_cli_handler).
-behavior(gen_event).

-export([ init/1, handle_event/2,
          terminate/2, handle_call/2, handle_info/2, code_change/3 ]).

init(_Args) ->
    {ok, []}.

handle_event(Msg, State) ->
    Indicator = case Msg of
        {_, planted, _} -> "[++]";
        {_, born, _}    -> "[++]";

        {_, eaten, _}   -> "[--]";
        {_, died, _}    -> "[--]";

        _               -> "[ii]"
    end,
    io:format("~s ~w ~n", [ Indicator, Msg ]),

    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, empty, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Args, _State) ->
    ok.
{% endhighlight %}

And the very important part in terms of the aforementioned complaints is that: when starting *event manager*, it is spawned as a process and each *event handler* is implemented as a callback module. But whole processing logic will be executed inside the same manager process.

## Why it is problematic?

Let me reiterate on that - after spawning `gen_event` manager and installing handlers on it, handlers exist in the same process as the manager.

That causes two biggest issues - handlers are not executed concurrently and they are not isolated from each other, in the process sense. But there is more - we heard explicitly that <em>I never used `gen_event`, I think it is a bad pattern</em> and whole argumentation about that can be summed by:

  - That aforementioned behavior it is not used anywhere besides `error_handler` and alerts mechanism in OTP.
  - It causes problems with supervision (because of not so natural approach for Erlang about combining manager and handlers together in one process).
  - It is tricky to use in fault tolerant way (as above - all handlers are bound together in single process).
  - It is tricky to manage state in manager, it may be tempting to use e.g. process dictionary, but you should be rather push it down to handlers (which is not obvious on the first sight).

So let's analyze the root causes of each complaint separately.

### Not widely used in the `erts` and `OTP`

First objection related to that behavior is that it is not widely used in the Erlang core libraries and platform itself. And that's partially true - as a behavior it is used for `error_logger`, `alarm_handler` and `error_handler` facilities. Is that a major reason to drop the behavior completely? No, but I think that it is a guide that responsibilities and use cases of that behavior are kind of limited, and much narrower than those we are trying to assign them.

### It is the same process for all handlers

This one was not explicitly stated on the list, but it manifests itself when it comes to failure handling and supervision. And also it has another, really significant drawback - which is obvious when you will think about it - all handlers are invoked synchronously and sequentially in one process.

In order to dispatch an event to manager you can use one of two `gen_event` functions - `notify` and `sync_notify`. With first you can dispatch event as quickly as possible, but you have no backpressure applied, and you can end up in the situation when events are incoming really fast, but processing is slower. That will cause process queue to grow and eventually it can cause even a crash. It does not check also the manager presence, so you can easily throw messages to the void. From the other hand - synchronous dispatch waits until event will be processed by all handlers, which can be slow and eventually will become a system bottleneck.

This problem is also very nicely described in the [Nick DeMonner talk](https://www.youtube.com/watch?v=yBReonQlfL4) from this year *ElixirConf US* conference - check this out if you are interested. Elixir `GenEvent` implementation has also third function - `ack_notify` which acknowledges the incoming messages, and it is something softer than `sync_notify`, but still asynchronous when it comes to processing.

### It is hard to supervise

When you are approaching Erlang as a newcomer and you are really fascinated by the mantra *everything should be a process*, the worst possible thing that can happen is to have some thoughts about event handling from other platforms or languages. Why? Well my *"oh crap"* moment about how things really work, came when I started an `observer`, and looked for the handler processes. And then I realized, <em>oh crap, they are not processes at all</em>.

This behavior hides the complexity underneath, and it has really good assumptions regarding that model of dispatching (if we separate handlers from manager, reliable dispatch is much harder to achieve e.g. when it comes to fault tolerance), but it is simply counterintuitive when it comes to the *Erlang* philosophy, especially for the newcomers.

### Failure handling

Another obvious thought when you realize that handlers and manager coexist in the same process is that: *What happen if there is a fault in the installed event handler module?*

It may sound strange at the beginning, but **faulty event handler will be silently removed**. It does produce an error report printed on terminal, but nothing more. Moreover, well known monitoring techniques, such as link or monitors cannot be used with the event handler module, because it is not a process. And a faulty event handler code does not crash the manager.

But we can use different facility exposed by `gen_event` called `add_sup_handler`. It means that the connection between process that wants to dispatch an event and the handler will be supervised. What does it mean? If the event handler is deleted due to a fault, the manager sends a message `{gen_event_EXIT, Handler, Reason}` to the caller. It means that we need to provide additional process, often called a *guard* for the possibly faulty handler. Then, dispatching of an event will happen through that *guardian* process, and when it receives the failure message (via `handle_info`) we can act accordingly to the requirements.

Keep in mind that underneath it uses *links*, not monitors - event handler chapter from [Learn You Some Erlang For Great Good!](http://learnyousomeerlang.com/event-handler) has really good explanation why it may be dangerous and what issues it causes. Long story short, after using `add_sup_handler` you need to be cautious when it comes to the event manager shutdown.

### State management

One more thing that I think is not emphasized enough is the state management and that you should always pass down state to your handlers. It is really well described in the example code above, but also when it comes to the fault tolerance - each handler can be removed due to failure operation and after restoring it we can pass the new state. If we will preserve state of that handler in the manager (and we will build facility for exposing that), it may cause strange and hard to debug side effects related with the state of the newly created handler.

## Alternatives?

Is there something that we can use instead? Without using third parties I am afraid that there is nothing like that in the core.

![GenRouter example from José Valim's presentation.](/assets/GenRouterExample.png)

If you are interested - in *Elixir* incoming *GenRouter* behavior looks really promising. Of course, it is still really far away from the core and its future is uncertain, but whole concept is described in the [José Valim's talk](https://www.youtube.com/watch?v=9RB1JCKe3GY) - there is even an example for that particular use case with `DynamicIn` - `BroadcastOut`, which will represent a process based replacement for `GenEvent`.

## Summary

If you think more wisely about that, it is not a particularly useful behavior, because it has very limited capabilities and responsibilities. Maybe that is the reason why it is used internally so rarely. It means also, that we should not bend it to our use cases. If the specific application is very similar to the one used inside *OTP* (I mean the `error_logger`) and we do not need concurrency support when it comes to the processing logic, we can safely use it. Otherwise, we incur troubles on ourselves. :wink:

### Credits

- [Erlang Documentation - Event handling principles](http://www.erlang.org/doc/design_principles/events.html)
- [Erlang Documentation - `gen_event`](http://www.erlang.org/doc/man/gen_event.html)
- [*Learn You Some Erlang for Great Good!* - Event Handlers](http://learnyousomeerlang.com/event-handlers)
- [Erlang Factory SF 2015 - The Timeless Way of Building Erlang Apps](https://www.youtube.com/watch?v=UUvU8cjCIcs)
- [ElixirConf 2015 - Keynote by José Valim](https://www.youtube.com/watch?v=9RB1JCKe3GY)
