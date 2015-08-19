---
layout: post
title: Seven Languages in Seven Weeks - Clojure
date: 2015-08-19T16:00+0200
categories:
  - 7-languages-in-7-weeks
tags:
  - series
  - programming-languages
  - clojure
  - books
---

# Seven Languages in Seven Weeks - Clojure

<quote class="disclaimer">This blog post is a next article from series related with books "Seven Languages in Seven Weeks" and its sequel. Each post will describe a single language chosen by this book and its most interesting and influencing features, from my point of view and previous experiences. I hope that you will find this series interesting. Do not hesitate with sharing your feedback and comments below!</quote>

### Introduction

We are heading to the end of the first book. We are almost there - there are only two positions left and today I would like to focus on another language which is based on the *JVM*.

In the book Bruce Tate assigns for each language a movie character (e.g. one of my favorite is [Agent Smith](https://en.wikipedia.org/wiki/Agent_Smith) associated with *Erlang*). Also the choice for *Clojure*, which is referred here, is brilliant - [Master Yoda](https://en.wikipedia.org/wiki/Yoda) speaks differently (like any Lisp-family based language), he is an exile (those languages waited very long for its fame and glory) and he has extraordinary knowledge.

<img class="right clojure-logo" alt="Clojure Logo" src="/assets/ClojureLogo.gif" />

### What is different in Clojure?

For me the key thing is that *Clojure* is a pragmatic language, and it is created by pragmatists.

Main example - it favors pure functions (with no *side-effects*), but also does not close you inside a very strict environment, where you cannot work with side-effects anymore. Instead it provides very natural and effective primitives for managing them.

Concurrency primitives based on *STM* (*Software Transactional Memory*) - like references and atoms, agents, or an amazing standard library for manging asynchronicity - `core.async` are another elements.

*Clojure* as a langauge has a unique approach to state manipulation, which is natural and not broken, unlike other models. It uses an *epochal time model*, a definition of state succession over time. A modified *state* is a value, which means it is immutable. But *identity*, which is the aforementioned succession of states, is a narrow view. The value inside that view differs depending on the time that observers chooses to reveal it.

The one of the main benefits for me, is also that *Clojure* runs not only on top of *JVM*. Even if the *CLR* port is not actively used, *ClojureScript* is becoming a huge thing. It really evolves, pushing many things forward (like the next version of [*Om*](https://github.com/omcljs/om) or other various React.js wrappers like [*Reagent*](https://github.com/reagent-project/reagent)). It also evolves in surprising directions - e.g. ability to create mobile applications in *Clojure* in a similar fashion to *React Native* ([more about that here](https://youtu.be/ByNs9TG30E8?t=2097) and [here](https://github.com/omcljs/ambly)).

Also there is one more point regarding the runtime platform - *JVM* and its evolution can be a limiting factor (you can observe that with e.g. lack of tail-call optimization for recursion, which is a key thing in functional programming languages). Using it as a platform has advantages and disadvantages - in other words - it is a double-edged sword. Thanks to its popularity, it brings many things to the table (community, experience, stable runtime and a huge collection of libraries) - but taking care about compatibility with this ecosystem is expensive and also evolution is slowed down by a *velocity* and sometimes *direction* of the runtime evolution. Keep in mind that it has a totally different momentum - the whole problem is very nicely explained by *Brian Goetz* [here](https://www.youtube.com/watch?v=2y5Pv4yN0b0).

*Clojure* is focused on data structures and their manipulation. It is our primary task in our daily job, every day we sort, transform, filter and shuffle data structures. Well defined and designed sequences (with support of lazy collections) and other more advanced constructs like *reducers* and *transducers* - everything helps you with one of the most common daily tasks. Internally, the implementation is also amazing - thanks to immutability we can fully leverage *structural sharing* for small and large data structures. Even if you decide to share your data with the external world, you can do it easily with *Clojure* style with [*`edn`*](https://github.com/edn-format/edn).

*Clojure* derives many things from *Lisp-family* languages - *macros* are one of those. Language creators gave you such amazing power, but also a responsiblity, to adapt and adjust language constructs. You can benefit from the language homoiconicity, create your own *DSLs* and so on. What is interesting, *Clojure* deliberately removed ability to define your own reader macros, in order to avoid creating your own and potentially incompatible *Lisp* dialect.

It is hard not to mention creator of the language - [*Rich Hickey*](https://twitter.com/richhickey). He is a brilliant designer and an insightful speaker. You are obliged to watch at least the [Simple made Easy](https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/SimpleMadeEasy.md) talk (personally my favorite, an eye-opener for me was *Clojure/Conj 2012* talk about [Language of the System](https://github.com/matthiasn/talk-transcripts/blob/master/Hickey_Rich/LanguageSystem.md)).

### We have other choices on the JVM...

Yes and I do not want to say that they are worse than *Clojure*. They are different and most of them (*Groovy*, *Kotlin*, *Java*) they are not adding anything new or revolutionary. Also, I have got a feeling that Scala, which was (still is?) a promising language, floats in a very unstable and dangerous direction - forks, difficult stewardship and adding too many features to the language, often in very chaotic way ([I wrote about that here](http://www.afronski.pl/7-languages-in-7-weeks/2015/06/29/seven-languages-in-seven-weeks-scala.html)) - everything adds up, I hope that it will not collapse until its own weight. Please, do not get me wrong - *Scala* is not a bad language, only some decisions made related with its future and development direction are slightly disturbing. 

### What is next?

And we have arrived almost to the end of the book. The last but not least is *Haskell* - strict and purist like *Spock* from *Star Trek* (yes, that is *Bruce's* choice). Then, after a short break, we will start again with the first language described in the sequel [*Seven More Languages in Seven Weeks*](https://pragprog.com/book/7lang/seven-more-languages-in-seven-weeks). See you in the next posts! :wink: Do not hesitate to share your feedback and comments below.

### Credits

- [Clojure, official site](http://clojure.org)
- [Rich Hickey's talk transcripts hosted by](https://github.com/matthiasn/talk-transcripts/tree/master/Hickey_Rich) [@matthiasn](https://github.com/matthiasn)
- [Seven Languages in Seven Weeks](https://pragprog.com/book/btlang/seven-languages-in-seven-weeks), *Bruce A. Tate*
