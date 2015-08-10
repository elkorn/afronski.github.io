---
layout: post
title: Seven Languages in Seven Weeks - Erlang
date: 2015-07-13T18:15+0200
categories:
  - 7-languages-in-7-weeks
tags:
  - series
  - programming-languages
  - erlang
  - books
---

# Seven Languages in Seven Weeks - Erlang

<quote class="disclaimer">This blog post is a next article from series related with book "Seven Languages in Seven Weeks" and its sequel. Each post will describe a single language chosen by this book and its most interesting and influencing features, from my point of view and previous experiences. I hope that you will find this series interesting. Do not hesitate with sharing your feedback and comments below!</quote>

### Introduction

<img class="right erlang-logo" alt="Erlang Logo" src="/assets/ErlangLogo.png" />

If you did not sleep under a rock in the past 30 years or this post is not a first one which you have read on this blog, you are probably familiar what *Erlang* is and which features are making it uniquely suited in certain class of applications. :wink:

I do not want to cover any interesting language features, because I have already written [several](http://www.afronski.pl/2015/03/10/interesting-language-features-erlang-named-case-expressions.html) [posts](http://www.afronski.pl/2015/03/27/interesting-language-features-erlang-links-and-monitors.html) [about](http://www.afronski.pl/2015/05/14/interesting-language-features-erlang-application-behavior.html) [that](http://www.afronski.pl/2015/06/11/interesting-language-features-erlang-custom-behaviors.html). Instead, I would like to introduce to you a small story about origin of the *actor model* in that particular case.

### A small story about the *actor model* in *Erlang*

The easiest way to start, will be to introduce definition directly from *Wikipedia*:

<quote class="citation">The actor model in computer science is a mathematical model of concurrent computation that treats "actors" as the universal primitives of concurrent computation: in response to a message that it receives, an actor can make local decisions, create more actors, send more messages, and determine how to respond to the next message received. The actor model originated in 1973. It has been used both as a framework for a theoretical understanding of computation and as the theoretical basis for several practical implementations of concurrent systems. The relationship of the model to other work is discussed in Indeterminacy in concurrent computation and Actor model and process calculi.</quote>

More or less at the same time, Erlang was created - with similar concepts inside. At first sight it looks like an industrial implementation of academic theory. However, creators did not call that an *actor model*, but all features are reflecting that idea pretty much entirely - core of this idea is a lightweight processes implementation, which use message passing for communication between them and complete isolation from each other.

It may sound as an impossible and strange coincidence, but that implementation **was not inspired by work of Hewitt, Bishop and Steiger at all**. As [Robert Virding](https://twitter.com/rvirding) said [here](http://rvirding.blogspot.com/2008/01/virdings-first-rule-of-programming.html?showComment=1400761539472#c5295780053912797163):

<quote class="citation">No, we didn't as such "invent" them, but at the same time we didn't take them from anywhere either. We arrived at these properties on our own as the best ways of solving our problems. We never heard about actors till years later.</quote>

It means that *actor model* has a solid industrial motivation and inspiration in the Telecommunication domain. Looking at this from a different perspective - it is how the phone calls are working at the highest conceptual level (also you can easily find analogies to that process in nature and biology). They have arrived to the same properties on their own, distilling and solving the domain problems over and over again - in complete isolation from the academic theory, as a best fit for set of their problems. I am convinced that this model is a valuable thing by that, because it means that at least two sets of people came up with same idea which is a perfect fit for handling certain class of problems related with concurrent operations.

### What is next?

We are heading inevitably to the end of first book - following blog posts in that series will cover two last programming languages - *Clojure* and *Haskell*. After that, we will have a short break, for one or two blog posts about other languages which are not covered in neither both aforementioned books. Then, we will start again with first language described in the sequel [*Seven More Languages in Seven Weeks*](https://pragprog.com/book/7lang/seven-more-languages-in-seven-weeks). See you next time then! :wink:

### Credits

- [Actor Model - Wikipedia](https://en.wikipedia.org/wiki/Actor_model)
- [A Universal Modular Actor Formalism for Artificial Intelligence - Hewitt, Bishop, Steiger](http://worrydream.com/refs/Hewitt-ActorModel.pdf)
- [Erlang - Official Website](http://www.erlang.org/)
- [Seven Languages in Seven Weeks](https://pragprog.com/book/btlang/seven-languages-in-seven-weeks), *Bruce A. Tate*
