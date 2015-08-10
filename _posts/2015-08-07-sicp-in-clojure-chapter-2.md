---
layout: post
title: SICP in Clojure - Chapter 2
date: 2015-08-07T16:00+0200
categories:
  - sicp-in-clojure
tags:
  - series
  - sicp
  - clojure
---

# SICP in Clojure - Chapter 2

<quote class="disclaimer">In one of the <a href="http://www.afronski.pl/2015/06/01/books-that-changed-my-career-structure-and-interpretation-of-computer-programs.html">previous blog posts</a> I have announced that I would like to start a new series of posts. It is a persistent journal from my journey through aforementioned book. I hope that you will enjoy it and find it useful - the main goal is to make this series a place where we can return in future, recall ideas and thoughts that accompanied reading process.</quote>

### Introduction

In the [previous blog post](http://www.afronski.pl/2015/06/04/sicp-in-clojure-chapter-1.html) we have started with an interesting example. In this case we will start with a small summary of the whole chapter and then we will move to couple very interesting examples.

Basically it is all about having clear and reasonable abstractions. Authors introduced a term called *barriers*, which help you building a contract and clear abstractions from the beginning. A separate place in the chapter is dedicated to the minimal syntax - which enables elasticity and freedom when constructing *new data types*, *type systems* and *domain specific languages.

The power of expression and also treating *code as data* has another benefit - you can easily transform your syntax, in order to change existing syntax to be more expressive, rewrite the human-friendly representation directly to the compiler-friendly one. 

Last but not least, composition and recursion is also important when it comes to the data structures, and building *compound data types*. And with that topic we will start.

### Compound Data Structures

{% highlight clojure lineos %}
;; Pair represented as a closure.
;;
;; We are returning new function, which
;; accepts only 0 or 1 as an index value.
;;
;; This is a constructor which is first part
;; of our barrier.

(defn cons [x y]
  (defn dispatch [m]
    (cond (= m 0) x
          (= m 1) y
          :else (assert (or (= m 1) (= m 0)) "Argument should be 0 or 1.")))
  dispatch)

;; Those functions are selectors, second
;; part of our barrier.

(defn car [z] (z 0))
(defn cdr [z] (z 1))

(println (car (cons 1 2))) ;; 1
(println (cdr (cons 1 2))) ;; 2

;; Pair represented as a list.
;;
;; We are returning list, which
;; has only 2 elements
;;
;; As in the previous case,
;; this is a constructor.

(defn cons [x y]
  (list x y))

;; In that case, following
;; functions are selectors.

(defn car [z] (nth z 0))
(defn cdr [z] (nth z 1))

(println (car (cons 1 2))) ;; 1
(println (cdr (cons 1 2))) ;; 2
{% endhighlight %}

We have presented here two implementations of *pair* data structure. First is build on top of list, later on top of *closure* which is a pretty standard technique when it comes to preserving small amount of state. The thing which I would like to highlight here is the very clean API focused around two types of function - *constructor* and *selectors*.

Thanks to that, we can exchange underlying implementation without any drawback for the end users. This kind of API oriented around data structure is our clean contract, a *barrier* which prevents clients from knowing how the data are actually organized. Moreover, it is still a pretty simple concept, based on primary elements available in the language - *functions* and *closures* in that particular case. We do not need to extend the language with additional concepts like e.g. *interfaces*.

### Barriers

Returning to the previous example - *barriers* are pretty natural concept when it comes to the functional programming. Taking *constructor* as an example - it is just an additional function, which have input arguments (a *contract*) and output (a *result*), which is a underlying representation of that particular data structure. It is a well known technique to all programmers, because on the lowest level it is just a normal function.

There is also two more things which are important when it comes to that term - first one is related with the abstractions, that can be build around certain representations. Imagine *collections* - all of them have some kind of notion of filtering, mapping over or reducing them. Clean *barriers* help build abstractions in much easier and clearer way for the end user.

Second thing is related with *responsibilities* and *anti-corruption layers* in your systems. Well defined barriers will focus on defining certain responsibilities in one place. In future, when you will change the underlying details, which should not be important for the rest of the system, *barriers* will protect those parts from being affected by that change.

### Freedom and Elasticity

{% highlight clojure lineos %}
;; First we define combinators like
;; `below`, `beside`, `flip-vert` and
;; `flip-horiz`.

;; Now we can define a `wave` which
;; draws a pattern.

;; Using previous combinators and patterns
;; we can easily build more complicated
;; patterns:

(defn wave2
  (beside wave (flip-vert wave)))

(defn wave4
  (below wave2 wave2))

;; You can even create more complicated
;; combinators from existing ones:

(defn right-split
  (split beside below))

(defn up-split
  (split below beside))
{% endhighlight %}

Thanks to the minimal syntax and small amount of syntactical rules it is easier to create something very expressive, meaningful for the users which will use the final form called *DSL* (*domain specific language*). Also, as you will see in the next section, it is very easy to transform it even further thanks to the very important feature of the language.

Even if the *Lisp-like* languages are dynamically typed, in almost all cases we are creating some form of *type hierarchy* or *type system*. It can be a benefit (in case of *DSL* we have less rules to obey and bend in order to introduce something useful and meaningful for the end-user) and also a drawback (some kinds of errors can be easily detected and handled by the basic type system level, which is built in the language). Keep in mind that it is a trade-off, there is no tool which is sufficient to cover all kind of use cases.

### Introduction to *homoiconicity*

{% highlight clojure lineos %}
;; Required selectors for extracting
;; data from assumed data structures.

(defn car [x] (first x))
(defn cdr [x] (rest x))
(defn cadr [x] (car (cdr x)))
(defn caddr [x] (car (cdr (cdr x))))

;; In our application pair is a:
;;   '(+ 1 2)

(defn pair? [x] (= (count x) 3))

;; Basic predicates.

(defn variable? [x]
  (symbol? x))

(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defn =number? [exp num]
  (and (number? exp) (= exp num)))

;; Custom constructors for sum and product.

(defn make-sum [a1 a2]
  (cond (=number? a1 0) a2
        (=number? a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list '+ a1 a2)))

(defn make-product [m1 m2]
  (cond (or (=number? m1 0) (=number? m2 0)) 0
        (=number? m1 1) m2
        (=number? m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list '* m1 m2)))

;; Predicate which detects sum.

(defn sum? [x]
  (and (pair? x) (= (car x) '+)))

;; Selectors for addition.

(defn addend [s]
  (cadr s))

(defn augend [s]
  (caddr s))

;; Custom predicate which detects product.

(defn product? [x]
  (and (pair? x) (= (car x) '*)))

;; Selectors for multiplication.

(defn multiplier [p]
  (cadr p))

(defn multiplicand [p]
  (caddr p))

;; Actual algorithm for symbolic derivation.
;; Please note how declarative this approach is,
;; how recursion actually helps to handle subsequent
;; cases and where the simplification mechanism is.

(defn deriv [exp var]
  (cond (number? exp)
         0
        (variable? exp)
         (if (same-variable? exp var) 1 0)
        (sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var))
        (product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp)))
        :else (assert false "Unknown expression type.")))
{% endhighlight %}

Basically almost anyone who is interested in the *Lisp-like* languages heard the term *homoiconicity*. If you don't know, this fancy word hides really simple concept - a property in which the program structure is similar to its syntax, and therefore the program's internal representation can be inferred by reading the text's layout. If the programming language is *homoiconic*, it means that the language text has the same structure as its abstract syntax tree. This allows all code in the language to be accessed and transformed as data, using the same representation.

The origin and one of the main reasons that *Lisp* was created is one related with the *symbolical processing*. In provided example we can easily operate on symbols (which represents an equation), which are introduced in the very same manner as the actual code - as a list of tokens, almost ready to invoke. Also we can easily transform a infix notation to the prefix notation with very simple helper function. The whole concept is just a small step before transforming an *AST* to a different form with e.g. *macros*.

Also, please note the way how we introduced the simplification (like `0 * x = 0` and `1 * y = y`) - it is easy to hide it from the user, that the final effect will be observed.

### Summary

In any kind of *Lisp* like languages many people complains about syntax - <em>too many parentheses</em>, <em>not enough constructs</em>, <em>weird look</em> are the most popular. Almost no people from that group are seeing that it can be a benefit - in terms of elasticity and expressiveness. Similar complaints are addressing other parts of the language (probably not in the *Clojure* case) like <em>too limited options regarding X</em> or <em>I need to write everything myself</em>.

I think that all of those questions are a trade-off between particular features which other systems / languages are giving us and drawbacks which are introducing with aforementioned functionality. In case of this book, maybe it is not particularly pragmatic to build a *pair* or *set* implementation from scratch, but it easier to explain functional design concepts with smaller examples.

### Credits

- [Structure and Interpretation of Computer Programs](http://mitpress.mit.edu/books/structure-and-interpretation-computer-programs), *Harold Abelson*, *Gerald Jay Sussman* and *Julie Sussman*
- [Full book available online](https://mitpress.mit.edu/sicp/full-text/book/book.html)
- [afronski/sicp-examples](https://github.com/afronski/sicp-examples)
