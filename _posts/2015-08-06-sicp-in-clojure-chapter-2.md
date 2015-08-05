---
layout: post
title: SICP in Clojure - Chapter 2
date: 2015-08-06T16:00+0200
---

# SICP in Clojure - Chapter 2

<quote class="disclaimer">In one of the <a href="http://www.afronski.pl/2015/06/01/books-that-changed-my-career-structure-and-interpretation-of-computer-programs.html">previous blog posts</a> I have announced that I would like to start a new series of posts. It is a persistent journal from my journey through aforementioned book. I hope that you will enjoy it and find it useful - the main goal is to make this series a place where we can return in future, recall ideas and thoughts that accompanied reading process.</quote>

### Introduction

In the [previous blog post](http://www.afronski.pl/2015/06/04/sicp-in-clojure-chapter-1.html) we have started with an interesting example. In this case we will start with a small summary of the whole chapter and then we will move to couple very interesting examples.

Basically it is all about having clear and reasonable abstractions. Authors introduced a term called *barriers*, which help you building a contract and clear abstractions from the beginning. A separate place in the chapter is dedicated to the minimal syntax - which enables elasticity and freedom when constructing *new data types*, *type systems* and *domain specific languages.

The power of expression and also treating *code as data* has another benefit - you can easily transform your syntax, in order to change existing syntax to be more expressive, rewrite the human-friendly representation directly to the compiler-friendly one. 

Last but not least, composition and recursion is also important when it comes to the data structures, and building *compound data types*. And with that topic we will start.

### Compound Data Structures

- Clean interface.
- Constructors and selectors.
- Thanks to the abstraction, after establishing a contract, it is easy to change implementation underneath.

### Barriers

- Conventional interface.
- Contracts.

### Freedom and Elasticity

- *DSL*
- Building type systems.
- Minimal syntax can result with introducing rules on the semantic level.

### Introduction to *homoiconicity*

- Operating on symbols.
- A small step before transforming an *AST*.

### *Interesting Examples*

- Rational numbers.
- Symbolical operations - derivation.
- Set implementation.
- Graphical language as a *DSL*.

### Credits

- [Structure and Interpretation of Computer Programs](http://mitpress.mit.edu/books/structure-and-interpretation-computer-programs), *Harold Abelson*, *Gerald Jay Sussman* and *Julie Sussman*
- [Full book available online](https://mitpress.mit.edu/sicp/full-text/book/book.html)
