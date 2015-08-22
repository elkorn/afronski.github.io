---
layout: post
title: Seven Languages in Seven Weeks - Scala
date: 2015-06-29T16:00+0200
categories:
  - 7-languages-in-7-weeks
tags:
  - series
  - 7-languages-in-7-weeks
  - programming-languages
  - scala
  - books
---

# Seven Languages in Seven Weeks - Scala

<quote class="disclaimer">This blog post is a next article from series related with books "Seven Languages in Seven Weeks" and its sequel. Each post will describe a single language chosen by this book and its most interesting and influencing features, from my point of view and previous experiences. I hope that you will find this series interesting. Do not hesitate with sharing your feedback and comments below!</quote>

### Introduction

<img class="right scala-logo" alt="Scala Logo" src="/assets/ScalaLogo.png" />

I have got a small sentiment for that language. After couple of years working primarily with *OOP* languages, I have tried something new. I enrolled into *Functional Programming Principles in Scala*. It was a *great choice* - the course itself was an amazing experience, I have learned a lot. Basically, it was my first functional language adventure - and I liked it (it may sound strange, but I have not, literally, even a single hour related to that topic during my studies in the college).

After that experience, I dived deeper into that topic. I have worked with other programming languages, I built more and more projects in that methodology - starting from smaller things to a bigger ones. I have read more and more books related with that topic. And my perspective also changed. In that blog post I would like to focus not on the language features, but some things which I consider, at the same time, the *advantages* and *curses* of Scala.

### "Object-Oriented Meets Functional"

As you probably know, *Scala* has both paradigms inscribed in its nature. It is easy and approachable for newcomers - it is often described as a *hybrid* or a *bridge* programming language. Another important advantage is that one related with a platform - it has seamless integration with Java and it brings whole power of JVM to the table.

The whole problem which I have with that approach is the following - it is really easy to start and employ certain functional constructs in the *OOP* world, but in order to start thinking in a purely functional manner you need to drop almost everything related with the conventional and mainstream *OOP* (which is - surprise, surprise - [a broken model](http://c2.com/cgi/wiki?AlanKaysDefinitionOfObjectOriented)). Is it hard to believe? Try to seek a *"truly"* object oriented stuff in [that book](http://www.manning.com/bjarnason/) - Good luck with that. :wink:

It is also impossible to enforce *pureness* and e.g. *immutability*. It is impossible on the compiler side to do it on the compiler side. It is possible to build another layer on top of that, but it is really hard to develop such safety net with a type system - and most people will consider this rather impractical. So it means, that the all good practices and good features attached to the language can be destroyed by a single decision (often caused by a laziness, pragmatism or convenience). *I do not think that it is a good idea* - I would rather go in a *"painful"* path with e.g. being immutable from the beginning.

### Feature Bloat

Another problem that I have with *Scala* it is its size - in terms of *features* and ways to *do something*. It is really hard to grasp all possible syntactical choices related with various constructs. Core of the language has really strange capabilities - my *"favorite"* one is built-in support for *XML* (I do not get why it is not defined in a user space, especially that *Scala* is well known from its relaxed syntax, which is really nice when it comes to the *DSL* creation). Why it does not have a JSON then? :wink:

The same thing applies to the *operator overloading* (please look at the *Scalaz* library - for a person not familiar with that library, code written in that manner is illegible) or different (and *strange*) syntactical rules e.g. related with [somersault of operands](https://www.agilelearner.com/presentation/81).

From the other hand *Scala* core language features does not have anything related with concurrency besides `Future[T]` and `promise[T]`. This is really a strange choice, especially looking at the *XML* support in the core. And I am not only whining here, because *Scala* has also many awesome features - the best ones are *Partial Functions* and *Traits*. It has also very decent *pattern matching* and impressive *type inference*. But the core language with libraries size it is definitely too big.

### JVM, oh sweet JVM...

[There is no doubt](http://www.tiobe.com/index.php/content/paperinfo/tpci/index.html), Java is still the most popular programming language - it automatically means that the *JVM* is very popular, and all languages based on that platform can easily employ that power. *Scala* also does that.

Mentioned popularity is even more visible when it comes to the books. [In 20015, 5 out of 6 books in the functional programming space are about *Scala*](https://youtu.be/HLCFJ9hnR1M?t=808). During last 4 years [Programming in Scala](http://www.artima.com/shop/programming_in_scala_2ed) was the most popular book in the functional programming space.

Basing design of a new language on top of the existing platform can be sometimes [a constraining path](http://docs.scala-lang.org/overviews/core/value-classes.html). The canonical example is [`scala.util.control.TailCalls`](http://www.scala-lang.org/api/current/#scala.util.control.TailCalls$). I hope that amount of work related with providing workarounds for `JVM` limitations is still smaller than the amount of actual work. And last but not least - tooling. I have nothing against *`scalac`* or *`REPL`*, but `sbt` - really? Are we still in XXI century, or did I miss something? :wink: In the ranking of the most obscure programming tools `sbt` will be on the podium.

### Summary

It does not mean that *Scala* is a bad programming language - it has huge minds behind, it has certain merits (even for me - it somehow introduced me to the topic of the *functional programming*), but it should provide something more - and by *more*, I do not mean more features. Without that it will be just another language on top of *JVM*, which brings some of functional concepts to the crowd - and it will stay in that way.

I think that one of such things that are different and brings real value to the community is the [Functional Programming in Scala](http://www.manning.com/bjarnason/) book. It is a masterpiece, I have not read it fully yet, but from the very first chapters I see that it brings thinking about *FP* on the different level. *This book is definitely recommended for everyone who is interested in functional paradigm*.

### What is next?

In the next blog post we will talk about my favorite programming language, *[Erlang](http://www.erlang.org)*. It has unique approach to the concurrency, it is a mature and battle-tested platform, which has still active and vibrant community (recently expanded by a fast growing *Elixir* community). See you in the next blog post! :wink:

### Credits

- [Scala](http://www.scala-lang.org/)
- [Programming in Scala, 2nd Edition](http://www.artima.com/shop/programming_in_scala_2ed)
- [Seven Languages in Seven Weeks](https://pragprog.com/book/btlang/seven-languages-in-seven-weeks), *Bruce A. Tate*
