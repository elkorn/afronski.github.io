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
  - books
---

# SICP in Clojure - Chapter 3

<quote class="disclaimer">In one of the <a href="http://www.afronski.pl/books-that-changed-my-career/2015/06/01/books-that-changed-my-career-structure-and-interpretation-of-computer-programs.html">previous blog posts</a> I have announced that I would like to start a new series of posts. It is a persistent journal from my journey through aforementioned book. I hope that you will enjoy it and find it useful - the main goal is to make this series a place where we can return in future, recall ideas and thoughts that accompanied reading process.</quote>

### Introduction

In the [previous blog post](http://www.afronski.pl/sicp-in-clojure/2015/08/07/sicp-in-clojure-chapter-2.html) (which had surprisingly good reception [here](https://news.ycombinator.com/item?id=10038515)) we have analyzed second chapter of the book. We did not know, that around the corner there is a ground breaking *"twist"* prepared by the authors in the 3rd chapter. We will face it today.

I should also mention that recently [SICP Distilled](http://www.sicpdistilled.com/) went publicly, which is also a very good tour on the book, with more idiomatic Clojure examples. Nevertheless, I am still going my way through the book and I have got couple of interesting observations from the aforementioned chapter. But first, I would like to address one question which was posted on the *Hacker News* thread.

### Clojure is not a Scheme and `SICP` does not help you to learn Clojure at all

Even if the first part is not debatable at all (how come one language could be identical to another one, even if they have common roots), then the second one is definitely dependent on the point of view. Mine is really obvious - *Clojure* is my first contact with a *Lisp-like* language. And book has much more to offer than a simple language course. I would say that language course and SICP can be complementary. How?

Basically (from my observations) aforementioned book is about *good software design* and how to approach *functional programming and design* in a *dynamic language*. It attacks various problems like *modularity*, *data structures*, and others also related with the *functional design*. It provides a good foundation for beginners, and I strongly believe that it can be beneficial for more advanced programmers. By taking that book and completely new language, I am treating this book as a playground - more like a reference and skeleton - which can be filled when I play with the language. Of course, it is not a tutorial how to approach a language correctly, in an idiomatic way. Instead, I am building references, analogies and comparisons, which opens a different mode when I am working strictly in a different way, with the new language. Also, I can focus on the learning by comparison - but I need to be careful, because instead learning new thing, I can mirror constructs which are natural only for *Scheme* in *Clojure*.

Probably because of that someone bring Rich Hickey's opinion about `SICP` to the table:

<quote class="citation">I personally don't think SICP will help you much with Clojure. YMMV.</quote>

And it is hard to argue with that as well - it is *true*, SICP has nothing to do with learning Clojure. But, I can refer to topics which I already know and thanks to that - learn Clojure at the same time, seeking by myself for an idiomatic path to describe the same thing, but in *a Clojure way*. Of course, there will be plenty of mistakes in the process - but hey, isn't that the most effective way to learn? :wink:

### Danger is hiding around the corner

Returning to the main topic - we went more than 200 pages through the book, we built various data structures, simple and not so simple programs which solve certain problems. And around the 215 page (in *Polish* edition :wink:) authors introduced concept of *mutable state*. You may think *"oh come on, we are dealing with that on the daily basis"*. Why it is dangerous? Let's look at the example:

{% highlight clojure linenos %}
{% endhighlight %}

Now, I would like to refer to some features that functions without mutable state have. Till now, our functions were fully and only dependent on the input arguments. Taking that values, and body of the function you could clearly reason about the result of the function. Output of that operation could be calculated with the simple substitution method. In other words - our programs were *referentially transparent* - we could substitute one part of the program with its calculated, simpler version and the result will be the same. In the example presented above we are not passing state from the previous invocation explicitly - it is buried inside the *computation object*.

By introducing concept of mutable state, our simple substitution methods are worthless. Functions are not only dependent on arguments, but also on previous state (*local* or even worse - *global* one). We need to deal with concept of *time* in our programs. Perfect, by that we just added one more variable in our environment. It means that, our simple invocation of our function can cause a *side effects*. Taking the previous term - our programs became *referentially opaque*.

### What is a *side effect*?

It is an additional behavior (often unwanted - either unaware or forgotten), somewhere in our environment related with the function invocation internals. It is either a mutation of some kind of state, or function invocation, which causes mutation. It means that *something* in our execution environment is different before and after the function call. Without analyzing body of the function we are not capable of defining what kind of change it is and where it happened. Pretty scary, huh? Well, welcome to the *imperative programming languages* world.

It does not mean that imperative programming languages are particularly bad, you can write pretty clean programs in those languages if you will stick to certain rules and guides. Moreover, all kind of *I/O* is a *side effect* - it does not mean that it is bad, but it changes the state - it mutates the environment. It has disadvantages and advantages (sometimes it is simpler to model some things in that way - e.g. `random` numbers generator, which relies on some mutable *seed* state). I should mention also that there are some languages which are really strict when it comes to the *I/O*, *side effects* and execution - one example is *Haskell* (described [here](http://www.afronski.pl/7-languages-in-7-weeks/2015/08/26/seven-languages-in-seven-weeks-haskell.html)). How they enforce strictness on that is a topic for another blog post - *Clojure* is different. **It is pragmatic**.

Language philosophy encourages you to write pure, functional core (by that I mean that functions are *referentially transparent*, without *mutable state* and *side effects*). But, it allows you in a very controlled and easy way deal with all kind of *side effects* and *impure* peripherals. We will define them later.

### Horrible consequence of mutability

There is one more consequence of mutability. If we will think for a moment, by introducing *mutable state*, we are introducing notions of *time* and *resource ownership* (someone is an owner of that mutable state, it does not flow from one call to another via input and output). It means that someone is an owner of a particular state, it can be changed there, but others can also read it. Or even worse - sometimes multiple actors can modify the state. In both cases *sharing* introduces some really nasty consequences, because it means that time and access to that resource need to synchronized between multiple parties. **And, as you can imagine, that causes awful lot of problems**.

### Concurrency and mutability in Clojure

*TODO*: defs, Vars - isolated, changing state between threads (binding).

{% highlight clojure linenos %}
{% endhighlight %}

*TODO*: Refs - shared, synchronous, coordinated (STM).

{% highlight clojure linenos %}
{% endhighlight %}

*TODO*: Atoms - shared, synchronous, independent.

{% highlight clojure linenos %}
{% endhighlight %}

*TODO*: Agents - shared, asynchronous, independent.

{% highlight clojure linenos %}
{% endhighlight %}

### Simulations with mutability

There is a very nice example presented in the book, which implements circuit board simulation. First authors implement it with use of mutable state. Let's look at part of that implementation (whole can be found [here](https://github.com/afronski/sicp-examples)):

{% highlight clojure linenos %}
{% endhighlight %}

*TODO*: Explain why it is wrong.

### Laziness

*TODO*: How to build lazy sequence, `lazy-seq`, `memoize`, data structures and sequences.

{% highlight clojure linenos %}
{% endhighlight %}

### Another approach to simulation - Streams

When you read previous section, you probably have a feeling that it must be a better way. Indeed, that is a *better way* to approach that problem - in the last section of the chapter authors are introducing new implementation of the *circuit board simulator*, which use *streams* and *laziness*:

{% highlight clojure linenos %}
{% endhighlight %}

*TODO*: Explain why it is better.

### Summary

It was a long article, with a lot of twists and groundbreaking truths. For developers with some experience (especially related with *multi-threading* and *concurrency*) it is probably a bread and butter. And probably by that we are unconsciously got used to it. Change and reflection about state of our tools requires fresh point of view. New perspective, I hope that by reading those kind of books I will easily get one. And thanks to that I will be able to rethink my daily practices, and obviously learn new stuff.

See you next time, in the penultimate, 4th chapter of the book! :wink:

### Credits

- [Structure and Interpretation of Computer Programs](http://mitpress.mit.edu/books/structure-and-interpretation-computer-programs), *Harold Abelson*, *Gerald Jay Sussman* and *Julie Sussman*
- [Full book available online](https://mitpress.mit.edu/sicp/full-text/book/book.html)
- [afronski/sicp-examples](https://github.com/afronski/sicp-examples)
- [SICP Distilled](http://www.sicpdistilled.com/)
