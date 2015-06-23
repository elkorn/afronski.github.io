---
layout: post
title: Seven Languages in Seven Weeks - Scala
date: 2015-06-29T16:00+0200
---

# Seven Languages in Seven Weeks - Scala

<quote class="disclaimer">This blog post is a next article from series related with books "Seven Languages in Seven Weeks" and its sequel. Each post will describe a single language chosen by this book and its most interesting and influencing features, from my point of view and previous experiences. I hope that you will find this series interesting. Do not hesitate with sharing your feedback and comments below!</quote>

### Introduction

<img class="right scala-logo" alt="Scala Logo" src="/assets/ScalaLogo.png" />

I have got small sentiment for that language. After couple of years working with *OOP* languages I have tried to something new. I enrolled into *Functional Programming Principles in Scala* - the course itself was an amazing experience, I have learned a lot. Basically, it was my first functional language encounter (it may sound strange but have literally not a single hour related to that topic during our studies) - and I liked it.

After that I dove into that topic deeper. I have worked with other programming languages, I built more and more projects - starting from smaller to bigger ones. I have read more and more books related with that topics. And my perspective also changed. In that blog post I would like to focus not on the language features, but something which I consider - at the same time - as the *advantages* and *curses* of Scala.

### Hybrid Programming Language

*TODO*

- (+) Easy to bridge for newcomers
- (-) Lack of enforcement regarding strictness (you can use `var` everywhere, how about that Mr. Compiler).
- (-) Object orientation argue with functional programming.

### Feature Bloat

*TODO*

- (+) Partial Functions - awesome, but what price?
- (-) XML - why not in user space?
- (-) Multiple ways of defining the same thing.
- (-) Lack of concurrency primitives built-in.
- (-) Operator overloading, look into Scalaz.

### JVM, oh sweet JVM...

*TODO*

- (+) Popularity...
  - [In 20015, 5 out of 6 books in the functional programming space are about *Scala*](https://youtu.be/HLCFJ9hnR1M?t=808)
  - During last 4 years [Programming in Scala](http://www.artima.com/shop/programming_in_scala_2ed) was the most popular book in the functional programming space.
- (-) has the price.
- (-) It directly influences language design.
- (-) Tooling - `sbt`, really? In XXI century?

### Summary

It does not mean that *Scala* is a bad language - it has huge minds behind, it has certain merits (even for me - it somehow introduced me to the topic of *functional programming*), but it should provide something more - I do not mean more features. Without that it will be just another language on top of *JVM*, which brings some of functional concepts to the crowd - and it will stay in that way.

I think that one of such things that are different and brings real value to the community is the [Functional Programming in Scala](http://www.manning.com/bjarnason/) book. It is a masterpiece, I have not read it fully yet, but from the very first chapters I see that it brings thinking about *FP* on the higher level. *This book is definitely recommended for everyone who is interested in functional paradigm*.

### What is next?

In the next blog post we will talk about my favorite programming language, *[Erlang](http://www.erlang.org)*. It has unique approach to the concurrency, it is mature and battle-tested platform, which has still active and vibrant community (recently expanded by a fast growing *Elixir* community). See you in the next blog post! :wink:

### Credits

- [Scala](http://www.scala-lang.org/)
- [Programming in Scala, 2nd Edition](http://www.artima.com/shop/programming_in_scala_2ed)
- [Seven Languages in Seven Weeks](https://pragprog.com/book/btlang/seven-languages-in-seven-weeks), *Bruce A. Tate*
