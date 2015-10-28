---
layout: post
title: What is wrong with gen_event?
date: 2015-11-02T16:00+0100
tags:
  - erlang
  - erlang-behaviours
  - patterns
  - programming-languages
---

# What is wrong with `gen_event`?

<quote class="citation">I never used gen_event, I think it is a bad pattern.</quote>

At first it may look like a controversial statement, but I heard a lot of those complaints from other people. Originally, I heard that exact statement during the presentation [made by Garrett Smith about pattern language](https://www.youtube.com/watch?v=UUvU8cjCIcs) - someone asked about that behavior at the end. More recently I heard similar thing in [José Valim's presentation about what will come next in Elixir](https://www.youtube.com/watch?v=9RB1JCKe3GY).

It confused me every time I heard that, so I wanted to investigate the topic more deeply. But before we will dive into reasons and explanations, let's recall what is the purpose of this behavior.

## What is `gen_event`?

OTP introduces two different terms regarding that behavior - an *event manager* and *event handler*.

Responsibility of *event manager* is being a named object which can receive events. An *event* can be, for example: an error, an alarm, or some information that is to be logged. Inside manager we can have 0, 1 or many *event handlers* installed. Responsibility of the handler is to process an *event*.

When the *event manager* is notified about an event, it will be processed by all installed handlers. The easiest way to imagine that is to think about manager as a sink for incoming messages and handlers different implementations which are writing messages to disk, database or terminal.

Another example can be taken from my implementation of Francesco Cesarini's assignment called [Wolves, Rabbits and Carrots simulation](https://github.com/afronski/wolves-and-rabbits-world-simulation). Main purpose of that task is to introduce concurrency, but internally it is a simulation - so certain events are happening, and they will be broadcasted to the rest of the interested entities.

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

One of the *handlers* implementation - `simulation_cli_handler` - is related with writing messages to the console. It is the actual `gen_event` behavior implementation, so all event handlers are the representatives of that abstraction:

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

And the very important part in terms of the aforementioned complaints is that: an *event manager* is implemented as a process and each *event handler* is implemented as a callback module. But whole logic it will be executed and instantiated inside that manager process.

## Why it is problematic?

- TODO: After creating `gen_event` and installing handlers on it, handlers are in the same process that gen_event is, they are not concurrent, they are not isolated.
- TODO: Garrett said explicitly "I never used gen_event, I think it is a bad pattern" and arguments that by:
  - It is not used anywhere besides `error_handler` and alerts mechanism in OTP.
  - It causes problems with supervision.
  - It is tricky to use in fault tolerant way.
  - It is tricky to manage state in manager, it may be tempting to use e.g. process dictionary. You should be rather push it down to handlers.
- TODO: My aside - try to do a synchronous event dispatching, horrible.

### It is hard to supervise

- TODO: Hides the complexity underneath (where are the handlers in `observer:start().`, oh it is not a process).

### Failure handling

- TODO: https://erlangcentral.org/wiki/index.php/Gen_event_behavior_demystified

### It is the same process for the all handlers

- TODO: Lack of concurrency!
- TODO: `notify`, `ack_notify` and `sync_notify` - Second one is a better version which applies back-pressure, but still runs asynchronously.

This problem is also very nicely described in the [Nick DeMonner talk](https://www.youtube.com/watch?v=yBReonQlfL4) from this year ElixirConf US. Check this out if you are interested.

### Not widely used in the core and `OTP`

- TODO: http://learnyousomeerlang.com/event-handlers
- TODO: `error_logger`
- TODO: `alarms`?

## Alternatives

- TODO: GenRouter for the rescue?
- TODO: Jose Valim talk - an incoming GenRouter with DynamicIn - BroadcastOut can be a process based replacement for GenRouter.

![GenRouter example from José Valim's presentation.](/assets/GenRouterExample.png)

## Summary

If you think more wisely about that, it is not a particularly useful behavior, because it has very limited capabilities and responsibilities. Maybe that is the reason why it is used internally so rarely. It means also, that we should not bend it to our use cases. If the specific application is very similar to the one used inside *OTP* (I mean the `error_logger`) and we do not need concurrency support when it comes to the processing logic, we can safely use it. Otherwise, we incur troubles on ourselves. :wink:

### Credits

- [Erlang Documentation - Event handling principles](http://www.erlang.org/doc/design_principles/events.html)
- [Erlang Documentation - `gen_event`](http://www.erlang.org/doc/man/gen_event.html)
- [*Learn You Some Erlang for Great Good!* - Event Handlers](http://learnyousomeerlang.com/event-handlers)
- [Erlang Factory SF 2015 - The Timeless Way of Building Erlang Apps](https://www.youtube.com/watch?v=UUvU8cjCIcs)
- [ElixirConf 2015 - Keynote by José Valim](https://www.youtube.com/watch?v=9RB1JCKe3GY)
