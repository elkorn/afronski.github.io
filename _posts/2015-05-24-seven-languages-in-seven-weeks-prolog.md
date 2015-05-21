---
layout: post
title: Seven Languages in Seven Weeks - Prolog
date: 2015-05-24T16:00+0200
---

# Seven Languages in Seven Weeks - Prolog

<quote class="disclaimer">This blog post is a next article from series related with books "Seven Languages in Seven Weeks" and its sequel. Each post will describe a single language chosen by this book and its most interesting and influencing features, from my point of view and previous experiences. I hope that you will find this series interesting. Do not hesitate with sharing your feedback and comments below!</quote>

### Introduction

<img class="right prolog-logo" alt="SWI Prolog Logo" src="/assets/SWIPrologLogo.png" />

In this blog post I would like to spent some time with one of the most interesting programming languages ever - *Prolog*. In *Bruce Tate's* book this language is compared to the *Raymond* from the *Rain Man* movie. For most people *Prolog* is mostly known as a language that answers `no` to everything. :wink: It has very nice characteristics and often it is used in various domains and applications when other languages miserably failed.

Logo on the right is representing the most popular *Prolog* distribution called *SWI Prolog*, but in this blog post we will take a slightly different approach - we will use *Erlog*, which is a *Prolog* implementation on top of *Erlang VM*. We will use *Elixir* as a *glue* for everything.


### Facts and Rules

The main advantage of *Prolog* is its declarative approach. You are building *knowledge base* by declaring *facts* and *rules* that connect all of them. .

*TODO*: Examples for facts and rules.

### Unification

If we use *unification* together with the previously mentioned elements, we will receive a place where *Prolog* shines the most. Basing on the provided facts and rules, basing on that it can effectively deduce missing parts.

### Constrained Logic Programming

*TODO*: Sudoku solver example.
*TODO*: Explanation.

### Credits

- [SWI Prolog](http://www.swi-prolog.org/)
- [afronski/erlog_sudoku_solver](https://github.com/afronski/erlog_sudoku_solver)
- [rvirding/erlog](https://github.com/rvirding/erlog) and [zkessin/erlog-server](https://github.com/zkessin/erlog-server)
- [Learn Prolog Now!](http://www.learnprolognow.org)
- [Seven Languages in Seven Weeks](https://pragprog.com/book/btlang/seven-languages-in-seven-weeks), *Bruce A. Tate*
