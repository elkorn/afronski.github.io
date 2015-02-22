---
layout: post
title: SICP in Clojure - Chapter 1
date: 2015-03-05T20:00:00Z-02:00
---

# SICP in Clojure - Chapter 1

## What is a *SICP* and why should you care?

*SICP* is an acronym which represents a classic, computer science book and stands for "Structure and Interpretation of Computer Programs". It is written by Harold Abelson, Gerald Jay Sussman and Julie Sussman and it is a base book for introductory course in programming on MIT. All examples are written in *Scheme*, which is a very popular *Lisp* dialect. Well, right now it is not so popular as Clojure, so I would like to convert prepared examples to that language instead. But, why this book is different? There are plenty books out there, which are doing a fairly well introduction to the programming, why do we even care? Because of two things.

It introduces you to the programming from the ground up, starting from the most basic concepts related with programming. Actually, if you are an experienced programmer, you will be disappointed by first few dozens of pages. It will not introduce anything new, however it is written in very gentle and interesting way - you may think "Well, at least it feels interesting when I am reading it". After first 50 pages, you can see the first benefits of that book - it introduces you to the topic from the functional programming paradigm side. Why this is important? Well, that is the second reason.

*Free lunch is over*. Last few decades we surf on top of huge wave directed by *Moore's Law*. Chips are getting smaller, faster and more effective in terms of power efficiency. But everything has its limits. Laws of physics are inexorable. We cannot bend the basic laws of nature, so instead making chips faster and smaller, we have to make *more* of them. And when I say *more*, I mean dozens, not only couple of them. In this case, not everything can be dropped on the operating system level and many of our programs should have notion *parallel execution* inside. But too many things related with *software engineering* today - programming languages, paradigms, solutions are oriented around mutable and shared state.

Metaphorically we may say that instead of getting profits from *Moore's Law*, we have to deal with *Amdahl's Law* now. And this, one of the basics truths about computer science, is also inexorable. And it says that the maximum speedup in parallel environment is strictly related with its sequential part. Obviously, some parts have to be sequential, because of the inherent complexity related with problem domain, however putting more and more constraints (related with mutability and shared state) increases this factor significantly. And in this case our parallel environment is used inefficiently, or which is even worse - we are blocking resources which can be utilized in other parts of the system.

Besides that, *functional programming* exceeds your toolbox and enhances your programming skills, by bending them and enforcing to use them in a different way. By adding value to your toolbox, it introduces many powerful elements such as *referential transparency*, *mathematical conciseness*, *immutability*, *composability*, designing and thinking about parts of the system which are *pure* (side-effect free) and *impure* (abstracting side-effects away into different constructs like *Monads*). Afterwards, these are much more important than an ability to execute your code in a *parallel environment*, in many cases for free. What is event more important, you will see that this is a positive side effect of the all previously mentioned features.

I would like to introduce some amazing concepts from this book by emphasising them in the series of articles posted on this blog. Starting from this one, I will speak about each chapter separately, mentioning important concepts, documented with a code samples written in *Clojure*. I will not focus on documenting things from the beginners point of view, but rather looking for analogies which an experienced programmer should understand. Let's start!

## Function as a *first-class citizen*

## *Just-enough* abstraction

## Gentle start into *composability*

## *Interesting Examples*
