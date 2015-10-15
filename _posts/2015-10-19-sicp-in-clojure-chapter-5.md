---
layout: post
title: SICP in Clojure - Chapter 5
date: 2015-10-19T16:00+0200
categories:
  - sicp-in-clojure
tags:
  - series
  - sicp
  - clojure
  - books
---

# SICP in Clojure - Chapter 5

<quote class="disclaimer">In one of the <a href="http://www.afronski.pl/books-that-changed-my-career/2015/06/01/books-that-changed-my-career-structure-and-interpretation-of-computer-programs.html">previous blog posts</a> I have announced that I would like to start a new series of posts. It is a persistent journal from my journey through aforementioned book. I hope that you will enjoy it and find it useful - the main goal is to make this series a place where we can return in future, recall ideas and thoughts that accompanied reading process.</quote>

### Introduction

We are heading to the end of the book. It is the last chapter, and in the [previous blog post](http://www.afronski.pl/sicp-in-clojure/2015/10/05/sicp-in-clojure-chapter-4.html) I have already mentioned that last two chapters are really specific. And that is true, especially in terms of 5th chapter's content.

### High-level Convenience

Using high-level languages have many benefits. In terms of *Clojure* and other *Lisp*-like languages I would start with an *automatic memory management* and *GC*, *various data structures* or various optimizations, like *tail-recursion* etc. We do not think very often about how it is implemented, especially at the lowest level - in the hardware.

Can you imagine how the hardware should look like, to be capable of running code written in programming language from *Lisp* family? This is the main topic of the last chapter. Authors are starting with basic theory related to *register machines* and ending with the recipe for building a *compiler*. This blog post will be mostly theoretical, and instead of code examples, and exercises related with a topic "How to build a *Clojure* compiler", we will take a peek *under the hood*, directly into the language implementation.

But first, let's bring some definitions to the table.

### What is a *register machine*?

*TODO*: Register Machine definition and relation to classical *Von Neumann* architecture in declarative, functional programming languages.
*TODO*: Register Machine image.

### How to force the classic hardware to play nicely with functional programming languages?

*TODO*: Various tricks and techniques how to achieve convenience of high-level language on lowest level (*hardware*).

#### Tail-recursion

*TODO*

#### Garbage Collection and Memory Management

*TODO*

#### Data Structures

*TODO*

### Laziness, Infinite Collections and Streams

*TODO*

### Deconstructing *Clojure*

*TODO*: Topics:

- Compiler - http://blog.guillermowinkler.com/blog/2014/04/21/decompiling-clojure-ii/
- Data Structures - http://www.mattgreer.org/articles/clojurescript-internals-vectors/

### Limitations of the `JVM`

*TODO*: Point out all *JVM* related limitations. http://wiki.jvmlangsummit.com/pdf/27_Hickey_clojure.pdf

### Summary

Over the course of five blog posts, we have went through whole book. We covered a lot of ground and prepared a lot of exercises from the book. We also learned *Clojure* during the process. If I have to choose the most interesting example from the book, I would go with the [symbolic derivation](https://github.com/afronski/sicp-examples/blob/master/chapters/2/2.3.2/deriv.clj) or [electronic circuit simulator](https://github.com/afronski/sicp-examples/blob/master/chapters/3/3.3.4/electronic-circuit.clj). I did not change my opinion about the book - I still think that it is **one of the most important publications for a computer scientist**. Or for a programmer, who is just eager to learn and know more. Especially about *functional programming*.

I hope that you have enjoyed the series as much as I did. :smile:

### Credits

- [Structure and Interpretation of Computer Programs](http://mitpress.mit.edu/books/structure-and-interpretation-computer-programs), *Harold Abelson*, *Gerald Jay Sussman* and *Julie Sussman*
- [Full book available online](https://mitpress.mit.edu/sicp/full-text/book/book.html)
- [clojure/clojure](https://github.com/clojure/clojure) - Source code.
- [Clojure, A Dynamic Programming Language for the JVM](http://wiki.jvmlangsummit.com/pdf/27_Hickey_clojure.pdf) - Slides from Rich Hickey's talk about *Clojure* internals.
- [afronski/sicp-examples](https://github.com/afronski/sicp-examples)
