---
layout: post
title: SICP in Clojure - Chapter 4
date: 2015-10-05T16:00+0200
categories:
  - sicp-in-clojure
tags:
  - series
  - sicp
  - clojure
  - books
---

# SICP in Clojure - Chapter 4

<quote class="disclaimer">In one of the <a href="http://www.afronski.pl/books-that-changed-my-career/2015/06/01/books-that-changed-my-career-structure-and-interpretation-of-computer-programs.html">previous blog posts</a> I have announced that I would like to start a new series of posts. It is a persistent journal from my journey through aforementioned book. I hope that you will enjoy it and find it useful - the main goal is to make this series a place where we can return in future, recall ideas and thoughts that accompanied reading process.</quote>

### Introduction

By finishing the [previous chapter](http://www.afronski.pl/sicp-in-clojure/2015/09/18/sicp-in-clojure-chapter-3.html) we learned more about *functional programming*, *designing* and *dealing* with stateful computation and a little bit about *laziness*. It was pretty much a *general purpose programming book* till now. Last two chapters of the book are really ... *Lispy*. :wink:

Chapter which will be discussed today is focused on *Lisp* core values built around *universal evaluator*, *homoiconicity* and *linguistic abstractions*.

### What is homoiconicity?

Very concise, I would say a *mathematical*, definition will explain it as a isomorphic relation between language *AST* (*Abstract Syntax Tree*) and its syntax. In more human friendly words - it is a property of a programming language in which the program structure is similar to its syntax. If such language is *homoiconic*, it allows all code in the language to be accessed and transformed as data, using the same representation - because *AST* is exactly the same as the language itself.

All languages from *Lisp* family have this property, also languages like [*Io*](http://www.afronski.pl/7-languages-in-7-weeks/2015/04/30/seven-languages-in-seven-weeks-io.html), *Julia* or [*Prolog*](http://www.afronski.pl/7-languages-in-7-weeks/2015/05/24/seven-languages-in-seven-weeks-prolog.html) also have this ability. Keep in mind that it does not mean that having a *macros system* in the language makes it *homoiconic*.

### Metalinguistic abstraction

Title of this section sounds like a difficult concept, where the core idea is really simple. Aforementioned abstraction is an ability to *create new languages*. We have done that previously (e.g. by creating various *DSL*, *Domain Specific Languages* as an examples). By the creation, authors also mean ability to *evaluate* (or *interpret*) constructs written in that newly created dialect, by calculating values from the prepared expressions. Program which does such thing is called *an evaluator* (or *interpreter*).

If we go one level deeper in the abstraction tree, a *metacircular evaluator* (or also often called a *metacircular interpreter*) is an evaluator written in the same language that it will interpret. It means that you can write an interpreter of any *Lisp* dialect in the chosen *Lisp* language dialect.

### Core of metacircular evaluator

*Clojure* REPL (actually any kind of *REPL*) is an *evaluator*. But also, our run-time environments are also built on top of such constructs.

In *Lisp*, the core of the evaluator is often called a `eval`-`apply` cycle. If we will dive into implementations presented in the book, we will immediately see a symmetry between them. Authors defined both of them as follows.

#### `eval`

<quote class="citation">
  To evaluate a combination (a compound expression other than a special form), evaluate the subexpressions and then apply the value of the operator subexpression to the values of the operand subexpressions.
</quote>

{% highlight clojure linenos %}
;; You can either evaluate quoted expressions
;; or strings, but keep in mind that string
;; does not have an AST-like structure by itself.
;; It needs to be parsed first (with a
;; `read-string`).

(eval '(let [a 10] (+ 3 4 a)))
(eval (read-string "(+ 1 1)"))
{% endhighlight %}

Evaluation means that we take fragment of the code (in form of a *quoted expression* or parsed from a string) and evaluate that, using all rules of the language. In other word it *calculates* the result of a certain expression. Keep in mind that a delivered expression is just a data structure - list of keywords, other tokens, and other data structures. **And it looks exactly the same as the language itself**. That is a practical implication of the homoiconicity.

#### `apply`

<quote class="citation">
  To apply a compound procedure to a set of arguments, evaluate the body of the procedure in a new environment. To construct this environment, extend the environment part of the procedure object by a frame in which the formal parameters of the procedure are bound to the arguments to which the procedure is applied.
</quote>

{% highlight clojure linenos %}
;; Result of executing both expressions is
;; exactly the same, but only the first one
;; is an application.
;;
;; Function application means that you have to
;; deliver all arguments upfront in a form of
;; collection.

(apply str ["str1" "str2" "str3"])
(str "str1" "str2" "str3")
{% endhighlight %}

At the first sight you will find that `apply` is only a strange syntax for a function invocation. But then, the obvious reflection strikes in - **it is exactly the opposite**. Function call is a syntax sugar on top of `apply` function. Moreover, having this function in your toolbox opens a different ways of thinking about invoking unknown functions, and build other concepts like *partial application* and *currying* based on that.

#### Combining both powers together

Process of interpreting a program is an interaction between them. How it looks like? Here is an excerpt from an implementation (again full version is inside [afronski/sicp-in-examples](https://github.com/afronski/sicp-examples/blob/master/chapters/4/4.1.1/eval-apply.clj)):

{% highlight clojure linenos %}
(defn my-eval [exp env]
  (cond (self-evaluating? exp) exp
        (variable? exp)        (lookup-variable-value exp env)
        (quoted? exp)          (text-of-quotation exp)
        (assignment? exp)      (my-eval-assignment exp env)
        (definition? exp)      (my-eval-definition exp env)
        (if? exp)              (my-eval-if exp env)
        (lambda? exp)          (make-procedure (lambda-parameters exp)
                                               (lambda-body exp)
                                               env)
        (do? exp)              (my-eval-sequence (do-actions exp) env)
        (cond? exp)            (my-eval (cond->if exp) env)
        (application? exp)     (my-apply (my-eval (operator exp) env)
                                         (list-of-values (operands exp) env))

        :else                  (assert false "Unknown expression in `my-eval`.")))

(defn my-apply [proc args]
  (cond (primitive-procedure? proc)
          (my-apply-primitive-procedure proc args)
        (compound-procedure? proc)
          (my-eval-sequence (procedure-body proc)
                            (extend-environment (procedure-parameters proc))
                            args
                            (procedure-environment proc))

        :else
          (assert false "Unknown procedure type in `my-apply`.")))
{% endhighlight %}

Even without exact definitions of the used functions, code is pretty self-explanatory. As we can see *evaluation* requires in certain cases an *application*, and *application* requires *evaluation* of function *body* and *arguments*. They are often expressed as a *yin-yang* symbol, because they are complement each other.

### Different evaluation models

Instead of *reimplementing* different evaluation models, I have prepared different examples of such, built on top of *Clojure* standard library (or sometimes with additional custom facility). We will start with the concept which we already know from the previous chapter.

#### Laziness

We have met this concept earlier already. In the previous chapter we worked with *streams* and infinite collections which simulate e.g. computation process. But built-in mechanisms in *Clojure* have much more to offer in that matter. We have already created some infinite collections, but let us remind how it works:

{% highlight clojure linenos %}
(import java.util.UUID)

(defn uuid-seq []
  (lazy-seq
    (cons (str (UUID/randomUUID))
          (uuid-seq))))

(println (take 3 (uuid-seq)))

; (b09b2a29-2cad-4cda-8e4c-8a9a5c136f05
;  8ece35e6-202f-4977-9987-7292239833e4
;  0a336e55-5e42-4312-87ea-24e86ba4311e)
{% endhighlight %}

First we have defining a `lazy-seq` then we use standard mechanism of constructing the collection from the first, *evaluated* element and the rest which evaluation will be deferred. What I mean by deferring? If you will try to put the first line inside a file (but not inside the *REPL* - it will force the evaluation) you will receive nothing:

{% highlight clojure linenos %}
; This returns a lazy collection, which
; is not evaluated yet.
(map inc [1 2 3 4])

; You can force evaluation either by
; enforcing simple run (and wait for
; side-effects) or return the result
; of the operation.

(dorun (map inc [1 2 3 4])) ; nil
(doall (map inc [1 2 3 4])) ; (2 3 4 5)
{% endhighlight %}

But it is not an only way of creating lazy sequences. You can use also `repeat`, `repeatedly`, `cycle` or `iterate` in a following way:

{% highlight clojure linenos %}
; `repeat` and `repeatedly` creates an infinite sequence
; either of elements or results of a function call. You can
; create infinite sequence or a limited one by passing an
; argument or not.

(println (str (clojure.string/join (take 5 (repeat "Na "))) "Batman!"))
  ; "Na Na Na Na Na Batman!"

(println (repeatedly 5 #(rand-int 100)))
  ; 34 23 12 1 23

; `cycle` returns a lazy collection with repetitions
; of a delivered collection.

(println (take 5 (cycle [1 2 3])))
  ; (1 2 3 1 2)

; `iterate` is a more generic constructor. It returns
; a lazy sequence, which has the following values:
;
;   x, f(x), f(f(x)), ...
;
; This also means, that used `f` functions should be
; *pure* (no side-effects).

(println (take 5 (iterate (partial * 3) 1)))
  ; (1 3 9 27 81)
{% endhighlight %}

But laziness can be also used in a different way.

#### Ambiguous operator

TODO: Example implementation here: https://github.com/abeppu/toychest, http://www.randomhacks.net/2005/10/11/amb-operator/

#### Logic programming

TODO: Logic Evaluator - implement examples and exercises in `core.logic` from *Clojure*.

### Summary

We have managed to finish 4th chapter of the book. In the last part we will attack problems with which we are already familiar, but on the lowest possible level. We will focus on hardware specifics of *Lisp* implementations, including designing constraints related with those topics.

I hope that we will meet there again! :smile:

### Credits

- [Structure and Interpretation of Computer Programs](http://mitpress.mit.edu/books/structure-and-interpretation-computer-programs), *Harold Abelson*, *Gerald Jay Sussman* and *Julie Sussman*
- [Full book available online](https://mitpress.mit.edu/sicp/full-text/book/book.html)
- [afronski/sicp-examples](https://github.com/afronski/sicp-examples)
- [Homoiconicity](https://en.wikipedia.org/wiki/Homoiconicity)
