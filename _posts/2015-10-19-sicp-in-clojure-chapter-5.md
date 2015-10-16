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

*TODO*: Register Machine:
  - Definition and relation to classical *Von Neumann* architecture in declarative, functional programming languages.
  - Register Machine image.
  - https://en.wikipedia.org/wiki/Register_machine
  - https://mitpress.mit.edu/sicp/full-text/book/book-Z-H-31.html#%_sec_5.1

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

So instead of building our own *Clojure* compiler, we will go under the hood for a while. Do not worry, we will not go too deep into the internals, but hopefully we will get a better understanding about the language itself.

#### Is *Clojure* / *ClojureScript* interpreted or compiled?

Answer is like an usual one - **it depends**. It can be loaded dynamically directly to the *`JVM`* via `load / eval` (it is called *a dynamic compilation*) or compiled *ahead of time* to the *Java bytecode*, and then loaded into *VM* (it is called *AOT compilation*). But even in the case of dynamic compilation, you are preprocessing everything to the *bytecode* representation - the only difference is that it does not land in the file (it is generated during the run-time). As you may expected it means that *VM* does not understand *Clojure* or even *Java* languages directly - *bytecode* is a collection of class files. Each one contains description of a class and methods, which have been translated to intermediate representation suitable for *`JVM`* (you can find details [here](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html)).

If we already have such code, then *`JVM`* *loads*, *links* and *initializes* class files provided during execution. During the first phase it finds a binary representation of the class and creates the *VM* internal representation from that. Then (during the *linking* phase) it adds prepared representations to the *`JVM`* state, and during that it also verifies is everything is structurally correct. At this point it also can resolve the symbolic references and it will prepare static fields, prefilled with the default variables. In the last step it invokes the initializers, but only when the class is needed (e.g. they are referenced by `new` or static method invocations, and obviously by associating it with the main class).

BTW. Sometimes I hear people complaining about the *`JVM`* start-up time, in terms of loading *Clojure* *`REPL`*' - it has been already improved a lot, but if someone is interested in the details and why the *Clojure* (and not actually *`JVM`*) takes so long to bootstrap, I encourage you to dive deeper into those articles - [1](http://blog.ndk.io/2014/02/11/jvm-slow-startup.html), [2](http://blog.ndk.io/2014/02/25/clojure-bootstrapping.html).

Situation is slightly simpler, when it comes to *ClojureScript* - it is always compiled to the *JavaScript* representation. But the compilation process is somehow interesting anyway. Under the hood, it uses *Google Closure Compiler* and if you are familiar with it, it requires a specific structure to be built in order preserve certain features, especially during the optimization phase. By leveraging the *libraries*, *Google Closure Modules*, specific dependency management, `cljsc` compiler can do amazing things - to name just one, *dead code analysis* which can prune properly even the external libraries code (assuming that external dependency somehow supports *Google Closure* as well). Obviously there is a trade-off - there are [limitations](https://developers.google.com/closure/compiler/docs/limitations?csw=1), and if you want to do the most advanced optimizations you have to be compliant with them. That is the way how compiler can assume certain things and then safely do optimizations.

If you want grab a brief overview what kind of optimizations it can apply, I strongly suggest you to follow the tutorial from [swanodette/hello-cljsc](https://github.com/swannodette/hello-cljsc/blob/master/src/hello_cljsc/core.clj) - at the end, there is a small example how enabling just `{:optimizations :simple}` can nicely optimize the initial source code. And there is still `{:optimizations :advanced}` mode. :wink:

#### Data Structures

- *TODO*: Data Structures:
  - http://www.mattgreer.org/articles/clojurescript-internals-vectors/
  - http://blog.higher-order.net/2009/02/01/understanding-clojures-persistentvector-implementation
  - http://hypirion.com/musings/understanding-persistent-vector-pt-1
  - http://wiki.jvmlangsummit.com/pdf/27_Hickey_clojure.pdf - Persistent Data Structures.

### Limitations of the `JVM`

*TODO*: Point out all *JVM* related limitations. http://wiki.jvmlangsummit.com/pdf/27_Hickey_clojure.pdf

### Summary

Over the course of five blog posts, we have went through whole book. We covered a lot of ground and prepared a lot of exercises from the book. We also learned *Clojure* during the process. If I have to choose the most interesting example from the book, I would go with the [symbolic derivation](https://github.com/afronski/sicp-examples/blob/master/chapters/2/2.3.2/deriv.clj) or [electronic circuit simulator](https://github.com/afronski/sicp-examples/blob/master/chapters/3/3.3.4/electronic-circuit.clj). I did not change my opinion about the book - I still think that it is **one of the most important publications for a computer scientist**. Or for a programmer, who is just eager to learn and know more. Especially about *functional programming*.

I hope that you have enjoyed the series as much as I did. :smile:

### Credits

- [Structure and Interpretation of Computer Programs](http://mitpress.mit.edu/books/structure-and-interpretation-computer-programs), *Harold Abelson*, *Gerald Jay Sussman* and *Julie Sussman*
- [Full book available online](https://mitpress.mit.edu/sicp/full-text/book/book.html)
- [clojure/clojure](https://github.com/clojure/clojure) - Source code.
- [swanodette/hello-cljsc](https://github.com/swannodette/hello-cljsc)
- [Clojure, A Dynamic Programming Language for the JVM](http://wiki.jvmlangsummit.com/pdf/27_Hickey_clojure.pdf) - Slides from Rich Hickey's talk about *Clojure* internals.
- [afronski/sicp-examples](https://github.com/afronski/sicp-examples)
