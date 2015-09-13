---
layout: post
title: SICP in Clojure - Chapter 3
date: 2015-09-15T16:30+0200
categories:
  - sicp-in-clojure
tags:
  - series
  - sicp
  - clojure
published: false
---

# SICP in Clojure - Chapter 3

<quote class="disclaimer">In one of the <a href="http://www.afronski.pl/books-that-changed-my-career/2015/06/01/books-that-changed-my-career-structure-and-interpretation-of-computer-programs.html">previous blog posts</a> I have announced that I would like to start a new series of posts. It is a persistent journal from my journey through aforementioned book. I hope that you will enjoy it and find it useful - the main goal is to make this series a place where we can return in future, recall ideas and thoughts that accompanied reading process.</quote>

### Introduction

In the [previous blog post](http://www.afronski.pl/sicp-in-clojure/2015/06/04/sicp-in-clojure-chapter-2.html) (which had surprisingly good reception [here](https://news.ycombinator.com/item?id=10038515)) we have analyzed second chapter of the book. We did not know, that around the corner there is a ground breaking truth prepared by the authors in the 3rd chapter. We will face it today.

Also I should mention that recently [SICP Distilled](http://www.sicpdistilled.com/) went publicly, which is also a very good source how the tour on the book should look like when we choose Clojure as our primary language of choice. Nevertheless, I am still going my way through the book and I have got couple of interesting observations from the following chapter. But first, I would like to address one question which was posted on the Hackernews thread.

### Clojure is not a Scheme, SICP does not apply to Clojure at all

Even if the first part is not debatable at all (how come one language should be similar to another, even if they have common roots), then second one is definitely dependent on the point of view. Mine is really easy - Clojure is my first encounter with a Lisp-like language and SICP has much more to offer than a language course. How those two statements will add up?

Basically, from my observations aforementioned book is about *good software design*. It attacks various problems like *modularity*, *functional design* etc. It provides a good head start for beginners, and I strongly believe that it has also very good things for more advanced programmers. By taking that book and completely new language for me, I am treating this book as a playground, a skeleton - which can be filled when I play with language. Of course, it is not a tutorial how to approach a language correctly in an idiomatic way. Instead I am building references, analogies and comparisons, which opens a different mode when I am working strictly in different way, with new language.

Also someone bring Rich Hickey's opinion on that:

<quote class="citation">I personally don't think SICP will help you much with Clojure. YMMV.</quote>

And it is hard to argue with that as well - it is true, SICP has nothing to do with learning Clojure. But I can refer to topics which I know and thanks to that - learn Clojure at the same time, seeking for an idiomatic way to describe the same thing, but in *a Clojure way*.

### Danger is hiding around the corner

### What is a *side effect*?

### Horrible consequences of mutability

### Concurrency in Clojure

### Simulations with mutability

### Laziness

### Another approach to simulation - Streams

*TODO*:
- Irreversible result of mutability.
  - What is a side effect?
  - Why assignment operator introduces side-effects?
  - Mutability breaks referential transparency and substitution model.
  - Concurrency with mutability.
  - Significance of time with concurrency.
  - Be pragmatic about mutability - side-effect free core, data transformations - and peripherals with side-effects and mutable, encapsulated local state.
- Concurrency in Clojure.
  - STM and Refs (shared, synchronous, coordinated)
  - Atoms (shared, synchronous, independent)
  - Agents (shared, asynchronous, independent)
  - How to represent critical sections and mutexes with that constructs.
- Streams and laziness.
  - Clojure version of that - `lazy-seq`.
- Examples - simulations, data structures, accounts with mutable state.
  - State-full computation objects.
  - Time-aware simulations and concurrent systems.

### Summary

### Credits

- [Structure and Interpretation of Computer Programs](http://mitpress.mit.edu/books/structure-and-interpretation-computer-programs), *Harold Abelson*, *Gerald Jay Sussman* and *Julie Sussman*
- [Full book available online](https://mitpress.mit.edu/sicp/full-text/book/book.html)
- [afronski/sicp-examples](https://github.com/afronski/sicp-examples)
