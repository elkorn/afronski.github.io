---
layout: post
title: Interesting Language Features - Erlang I
date: 2015-03-10T16:00:00Z-02:00
---

# Interesting Language Features - Erlang I

<quote class="disclaimer">This blog post is a starting point of a series which contains examples, explanation and details of interesting programming language features. This is my private ranking, each post contains a good dose of my private opinions (you have been warned :wink:), but besides that I hope that you will find it valuable.</quote>

### Introduction

*TODO: Introduction*.

- Why *Erlang*?
- Obviousness, Context, Clear intention.
- Clean Code.
- Instead if, switch use power of declarative style.
- Describe place that we can find this code.

### Pattern Matching

*TODO: Pattern Matching explanation*.

{% highlight erlang linenos %}
% Is a character whitespace?

is_whitespace($\s)   -> true;
is_whitespace($\t)   -> true;
is_whitespace($\n)   -> true;
is_whitespace($\r)   -> true;
is_whitespace(_Else) -> false.
{% endhighlight %}

<h3><i>Named Case Expressions</i></h3>

*TODO: Presentation of case expression*.

{% highlight erlang linenos %}
-module(before).
-export([ dict_find/3 ]).

dict_find(Key, Dict, DefaultValue) ->
  case dict:find(Key, Dict) of
    {ok, Value} ->
        Value;
    error ->
        DefaultValue
  end.
{% endhighlight %}

But we can do it better, in more *Erlangish* way. *TODO: Why it is better*.

After a small refactoring:

{% highlight erlang linenos %}
-module(after).
-export([ dict_find/3 ]).

valueOrDefault({ok, Value}, _) -> Value;
valueOrDefault(_, Default)     -> Default.

dict_find(Key, Dict, DefaultValue) ->
  valueOrDefault(dict:find(Key, Dict), DefaultValue).
{% endhighlight %}

Lets look what happened here. *TODO: Detailed explanation*.

But what about performance? Would not it be slower because additional functions introduced here? We can check it easily, but digging deeper into *Erlang VM* internals.

### Internal representation (*Core Erlang*)

Before we will take a peek under the hood, we need to

{% highlight erlang linenos %}
% Function 'valueOrDefault' after compilation
% to the Core Erlang representation,
% used by BEAM internally:

'valueOrDefault'/2 =
    %% Line 4
    fun (_cor1,_cor0) ->
	case <_cor1,_cor0> of
	  <{'ok',Value},_cor4> when 'true' ->
	      Value
	  %% Line 5
	  <_cor5,Default> when 'true' ->
	      Default
	end
{% endhighlight %}

As you may see, *multi-head function clauses* are anyway compiled to the *case expression*, so argument about lost performance or additional overhead introduced by this abstraction is invalid.

How we took a peek under the hood? It is simple:

{% highlight erl %}
1> c(after, to_core).
ok
2> % Compiler will spit out the
   % Core Erlang representation
   % to the 'after.core'.
{% endhighlight %}

### Summary

*TODO: Nice summary about clearness, intention and obviousness which should be the primary goal*.

### Credits

- [Garrett Smith, *Social Code* (Code Mesh 2014)](https://www.youtube.com/watch?v=UuHMaeO1k-E)
- [CouchDB - *couch_util.erl*](https://github.com/apache/couchdb-couch/blob/master/src/couch_util.erl)
