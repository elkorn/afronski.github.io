---
layout: post
title: Seven Languages in Seven Weeks - Prolog
date: 2015-05-24T22:00+0200
---

# Seven Languages in Seven Weeks - Prolog

<quote class="disclaimer">This blog post is a next article from series related with books "Seven Languages in Seven Weeks" and its sequel. Each post will describe a single language chosen by this book and its most interesting and influencing features, from my point of view and previous experiences. I hope that you will find this series interesting. Do not hesitate with sharing your feedback and comments below!</quote>

### Introduction

<img class="right prolog-logo" alt="SWI Prolog Logo" src="/assets/SWIPrologLogo.png" />

In this blog post I would like to spent some time with one of the most interesting programming languages ever - with *Prolog*. In *Bruce Tate's* book this language is compared to the *Raymond* from the *Rain Man* movie. For most people *Prolog* is mostly known as a language that answers `no` to everything. :wink: It has very nice characteristics and often it is used in various domains and applications when other languages miserably failed.

Logo on the right is representing the most popular *Prolog* distribution called *SWI Prolog*, but in this blog post we will take a slightly different approach - we will use *Erlog*, which is a *Prolog* implementation on top of *Erlang VM*. We will use *Elixir* as a *glue* for everything.

### Facts, Relations, Rules and Queries

The main advantage of *Prolog* is its declarative approach. You are building *knowledge base* by declaring *facts* and *relations* that connect all of them together. Then, you can define *rules* with which you can query this *knowledge base* and retrieve information, like in the example below:

{% highlight prolog linenos %}
loves(vincent, mia). 
loves(marsellus, mia).
loves(pumpkin, honey_bunny).
loves(honey_bunny, pumpkin). 
    
jealous(X, Y):- loves(X, Z), loves(Y, Z).
{% endhighlight %}

In this example `loves` defines a *fact* between two entities (in our case represented by *atoms*, a unique symbols) - so we defined *relations* there. At the *6th* line we defined rule, which uses previous relations between two facts, represented as variables `X` and `Z` (as in the *Erlang* variables should be started with a *capital letter*).

Then if we *query* such database basing on rule `jealous` we will receive:

{% highlight prolog linenos %}
?-  jealous(marsellus, W).
W = vincent.
{% endhighlight %}

Which obviously true - fans of <i>*Pulp Fiction*</i> will already know why. :wink:

### Unification

If we use *unification* together with the aforementioned elements, we will receive a place where *Prolog* shines the most. Building on top of a pile of provided facts, relations and rules, it can effectively deduce missing parts. How? We already used it in previous example, but let's look at the example:

{% highlight prolog linenos %}
?- append([1], [2], L).
L = [1, 2].

?- append(W, [2], [1, 2]).
W = [1].

?- append(W, [3], [1, 2]).
false.
{% endhighlight %}

First query is a pretty much obvious *array concatenation* (in *Prolog* you have to return value by the parameter). But, in the second case something *strange* happened - interpreter responded how it should look first argument of a function call, if we want to receive `[1,2]` as a result. Third example is a similar case - it is not possible to substitute any value under `W` in order to satisfy these conditions.

### Sudoku Solver

So, let's try to use our knowledge in practice. Inside my small pet project - [afronski/erlog_sudoku_solver](https://github.com/afronski/erlog_sudoku_solver) - I have used *Erlog* interpreter on top of *Erlang VM*. Unfortunately there is no module similar to the `clpfd` which is available in the *SWI Prolog* distribution, so the Sudoku solver example will be a little more complicated and it will solve smaller boards - only `4x4`.

{% highlight prolog linenos %}
sudoku(Cells) :-
  Cells =
  [
      [A1,A2,A3,A4],
      [B1,B2,B3,B4],
      [C1,C2,C3,C4],
      [D1,D2,D3,D4]
  ],

  Possible = [1,2,3,4],

  pick_value(Possible, Possible, A1, RowA_234, Col1_BCD),
  pick_value(RowA_234, Possible, A2, RowA__34, Col2_BCD),
  pick_value(RowA__34, Possible, A3, RowA___4, Col3_BCD),
  pick_value(RowA___4, Possible, A4, _RowA___, Col4_BCD),

  pick_value(Possible, Col1_BCD, B1, RowB_234, Col1__CD), A2 \= B1,
  pick_value(RowB_234, Col2_BCD, B2, RowB__34, Col2__CD), A1 \= B2,
  pick_value(RowB__34, Col3_BCD, B3, RowB___4, Col3__CD), A4 \= B3,
  pick_value(RowB___4, Col4_BCD, B4, _RowB___, Col4__CD), A3 \= B4,

  pick_value(Possible, Col1__CD, C1, RowC_234, Col1___D),
  pick_value(RowC_234, Col2__CD, C2, RowC__34, Col2___D),
  pick_value(RowC__34, Col3__CD, C3, RowC___4, Col3___D),
  pick_value(RowC___4, Col4__CD, C4, _RowC___, Col4___D),

  pick_value(Possible, Col1___D, D1, RowD_234, _), C2 \= D1,
  pick_value(RowD_234, Col2___D, D2, RowD__34, _), C1 \= D2,
  pick_value(RowD__34, Col3___D, D3, RowD___4, _), C4 \= D3,
  pick_value(RowD___4, Col4___D, D4, _RowD___, _), C3 \= D4,

  true.

pick_value(RowVals, ColVals, Value, RowValRest, ColValRest) :-
   pickValue(RowVals, Value, RowValRest),
   pickValue(ColVals, Value, ColValRest).

pickValue([H|Rest], H, Rest).
pickValue([H|Tail], Picked, [H|Rest]) :- pickValue(Tail, Picked, Rest).

solve(L) :-
    L = [ [  _, 2, _, 4  ], [  _, 3, _, _  ], [  _, _, _, 1  ], [  _, _, 2, _  ] ],
    sudoku(L).
{% endhighlight %}

**And that is it**. As you may noticed - we defined only rules for the game, rest is done thanks to the *backtracking algorithms* implemented in the interpreter, which are searching and pruning all paths build on top of *knowledge base* made from *facts*, *relations* and *rules*. This example can be even more concise and clear, if you have module like [`clpfd`](http://www.swi-prolog.org/man/clpfd.html), as I mentioned before - you can find an example which uses aforementioned module [here](https://github.com/afronski/playground-other/blob/master/prolog/sudoku-resolver/sudoku-resolver.pro).

### Summary

In the next blog post we will talk about relatively new (compared to the other languages described in the book), but very popular *hybrid* programming language called *[Scala](http://www.scala-lang.org)*. It was my starting point with world of functional programming languages, and it is also often recommended as a starting point - especially if you have background as an *object oriented programmer*. See you soon! :wink:

### Credits

- [SWI Prolog](http://www.swi-prolog.org/)
- [afronski/erlog_sudoku_solver](https://github.com/afronski/erlog_sudoku_solver)
- [rvirding/erlog](https://github.com/rvirding/erlog) and [zkessin/erlog-server](https://github.com/zkessin/erlog-server)
- [Learn Prolog Now!](http://www.learnprolognow.org)
- [Seven Languages in Seven Weeks](https://pragprog.com/book/btlang/seven-languages-in-seven-weeks), *Bruce A. Tate*
