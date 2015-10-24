---
layout: post
title: What is wrong with gen_event?
date: 2015-11-02T16:00+0200
tags:
  - erlang
  - erlang-behaviours
  - patterns
  - programming-languages
---

# What is wrong with `gen_event`?

<quote class="citation">I never used gen_event, I think it is a bad pattern.</quote>

At first it may look like a controversial statement, but I heard a lot of those complaints from other people. Originally, I heard that exact statement in the presentation [made by Garrett Smith about pattern language](https://www.youtube.com/watch?v=UUvU8cjCIcs) during the question time. More recently I heard similar thing in [José Valim's presentation about what will come next in Elixir](https://www.youtube.com/watch?v=9RB1JCKe3GY).

It baffles me every time I hear that, so I would like to investigate the topic more deeply. But before we will dive into reasons and explanations, let's recall what is the purpose of this behavior.

## What is `gen_event`?

- TODO: Event manager.
- TODO: Event handlers.

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

It is not a particularly useful behavior, it has very limited capabilities and responsibilities. Maybe that is the reason why it is used internally so rarely. It means also, that we should not bend it to our use cases, because it will cause different issues. If the particular use case is very similar to the one used inside *OTP* (I mean the `error_logger`) and we do not need concurrency when it comes to the handlers processing logic, we can safely use it. Otherwise, we are asking for trouble. :wink:

### Credits

- [Erlang Documentation - Event handling principles](http://www.erlang.org/doc/design_principles/events.html)
- [Erlang Documentation - `gen_event`](http://www.erlang.org/doc/man/gen_event.html)
- [*Learn You Some Erlang for Great Good!* - Event Handlers](http://learnyousomeerlang.com/event-handlers)
- [Erlang Factory SF 2015 - The Timeless Way of Building Erlang Apps](https://www.youtube.com/watch?v=UUvU8cjCIcs)
- [ElixirConf 2015 - Keynote by José Valim](https://www.youtube.com/watch?v=9RB1JCKe3GY)
