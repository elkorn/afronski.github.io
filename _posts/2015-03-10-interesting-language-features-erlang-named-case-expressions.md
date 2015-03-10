---
layout: post
title: Interesting Language Features - Erlang I
date: 2015-03-10T16:00:00Z-02:00
---

# Interesting Language Features - Erlang I

<quote class="disclaimer">This blog post is a starting point of a series which contains examples, explanations and details about interesting features of various programming languages. I have collected several examples of different characteristics, which definitely extended my view regarding programming, architecture and structure in general. I would love to hear your feedback about presented choices or description of yours favorite programming language feature.</quote>

### Introduction

Almost every programmer struggled in the past with the unclear and non-obvious code, probably written yesterday or even earlier. That feeling is often amplified by the actual mental state (e.g. you are angry, not rested or upset - if you don't know how it affects you, I encourage you to get familiar with [this book and blog post](http://www.afronski.pl/2015/03/07/books-that-changed-my-career-pragmatic-thinking-and-learning.html)). If you will run onto some unclear and ugly written code sample, which yesterday was perfectly clear, your attitude may only be worse. It can escalate, if such code was written by someone else from your team.

*Software Craftsmanship* movement and Uncle Bob's books (especially [Clean Code](http://www.amazon.com/Clean-Code-Handbook-Software-Craftsmanship/dp/0132350882/) and [Clean Coder](http://www.amazon.com/Clean-Coder-Conduct-Professional-Programmers/dp/0137081073/)) are trying to improve that state, but most of mainstream programming languages did not encourage developers to write something clear and obvious at the first sight. Often context is hidden inside pile of code. Somewhere underneath unnecessary abstraction you can find the essence of that particular slice. Not to mention stateful programming languages, where often state is passed between following methods (or functions) inexplicitly.

Very long, tangled, imperative flow control can bury the code conciseness and clarity very easily (yes, I am talking about `if-else` and `switch-case` statements). If I had a dime for every time I lost track somewhere in the thicket of flow control statements, I could easily retire and write for a rest of my life *Haskell* for fun. But I would like to present very clear alternative, present in almost any modern programming language, especially in the functional ones.

### Pattern Matching

Instead of codifying rules imperatively, try to think about rules in a declarative way instead. I would like to focus on *Erlang* syntax right now, but still this concept can be represented in any functional programming language.

All examples of source code are gathered from the *CouchDB* project (very nice *key-value* storage with *JSON* documents stored as value, that *embraces the web*, written in *Erlang*). You can find link to the exact file in the [Credits](#credits) section:

{% highlight erlang linenos %}
% Is character a whitespace?
%
% Syntax $\s means a character
% represented as with escape sequence.
%
% Equal to the '\s' in C-like syntax.

is_whitespace($\s)   -> true;
is_whitespace($\t)   -> true;
is_whitespace($\n)   -> true;
is_whitespace($\r)   -> true;
is_whitespace(_Else) -> false.
{% endhighlight %}

You can see that we declared several cases what our function should return under certain input conditions. It looks like a function with multiple heads, each with different inputs, treated as a single case. All rules are checked in the declaration order, last one is a *catch-all* clause. As you can see, this function is very simple, but it is a good starting point to get familiar with the construct.

We can push it even further. Imagine that you can differentiate behavior and execution flow in such declarative way - lets see how we can do that.

<h3 id="named_case_expressions"><i>Named Case Expressions</i></h3>

Of course in *Erlang* and other languages there are *if* and *case* expressions (there is a significant difference between statement and expression, but that is another topic) and you can wrote your code in classical way:

{% highlight erlang linenos %}
-module(before).
-export([ dict_find/3 ]).

dict_find(Key, Dict, DefaultValue) ->
  case dict:find(Key, Dict) of
    {ok, Value} ->
        Value;
    _ ->
        DefaultValue
  end.
{% endhighlight %}

It is still a pattern matching - we are matching against a *tagged tuple* with `ok` atom at the first place, we have *catch-all* clause. But we can definitely do it better, in more *Erlangish* way. After a small refactoring code will look like this:

{% highlight erlang linenos %}
-module(after).
-export([ dict_find/3 ]).

getValueOrDefault({ok, Value}, _) -> Value;
getValueOrDefault(_, Default)     -> Default.

dict_find(Key, Dict, DefaultValue) ->
  getValueOrDefault(dict:find(Key, Dict), DefaultValue).
{% endhighlight %}

Lets look what happened here in details. We have changed the case expression into a function with clear name, that has two arguments - first is a result of `dict:find` and second is a default value. Then with pattern matching we are dispatching the execution flow - in our case we are returning a proper value (with extraction from a *tagged tuple*), but you can easily imagine that another function calls can be easily introduced here. We transformed *case* expression (a syntactical construct) to the *named* representation (a logical construct).

*Why it is better?* I hope that you can see that it is clearer at the first sight. Matching rules are stored in a declarative way, we have less syntactical noise and the most important thing - *it has a name*. That is the reason from where name of this *pattern* came from - *named case expressions*.

But what about performance? Would not it be slower, because additional functions introduced here? We can check it easily, by digging deeper into *Erlang VM* internals.

### Internal representation (*Core Erlang*)

Before we will take a peek under the hood, we need to briefly explain the structure of the *Erlang* compiler. Before the *Erlang* gets interpreted on the VM it is transformed to the *Core Erlang* representation and after that to the *BEAM* code (which is a equivalent of *bytecode* from *JVM*). It turns out that our *named case expressions* (functions with multiple heads and pattern matching, a logical construct) are directly transformed to the syntactical construct - *case* expression - in the first phase of compilation.

{% highlight erlang linenos %}
% Function 'getValueOrDefault' after compilation
% to the Core Erlang representation, which will be
% afterwards compiled to the BEAM code:

'getValueOrDefault'/2 =
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

As you may see, *multi-head function clauses* are anyway compiled to the *case* expression, so argument about lost performance or additional overhead introduced by this abstraction is simply invalid - there is no such overhead, we can use it without any doubts (in most cases such objections are simply the [premature optimization](http://en.wikipedia.org/wiki/Program_optimization) anyway).

How we took a peek under the hood? It is simple, you can start new *Erlang* shell and then execute:

{% highlight erlang %}
% Compiler will spit out the
% Core Erlang representation
% to the 'after.core' file.

c(after, to_core).
{% endhighlight %}

### Summary

Clear, concise and obvious code should be our goal from the beginning. It is easy to forget about it in the daily routine or rush caused by deadlines. It is a really small investment put upfront (even if it is bigger at the beginning, cost will definitely lower as time passes and we are gaining experience). It eases the debugging and *context reload* time. We should strive to simplicity, because *simple is better* - and it is worth knowing these tricks that can help you with pursuit to the final goal.

### Credits

- [Garrett Smith, *Social Code* (Code Mesh 2014)](https://www.youtube.com/watch?v=UuHMaeO1k-E)
- [CouchDB - *couch_util.erl*](https://github.com/apache/couchdb-couch/blob/master/src/couch_util.erl)
