---
layout: post
title: SICP in Clojure - Chapter 1
date: 2015-06-04T11:00+0200
categories:
  - sicp-in-clojure
tags:
  - series
  - sicp
  - clojure
---

# SICP in Clojure - Chapter 1

<quote class="disclaimer">In one of the <a href="http://www.afronski.pl/books-that-changed-my-career/2015/06/01/books-that-changed-my-career-structure-and-interpretation-of-computer-programs.html">previous blog posts</a> I have announced that I would like to start a new series of posts. It is a persistent journal from my journey through aforementioned book. I hope that you will enjoy it and find it useful - the main goal is to make this series a place where we can return in future, recall ideas and thoughts that accompanied reading process.</quote>

### What is a *SICP* and why should you care?

*SICP* is an acronym which represents a classic computer science book and stands for *"Structure and Interpretation of Computer Programs"*. It is written by Harold Abelson, Gerald Jay Sussman and Julie Sussman. It is a base book for [an introductory course in programming on MIT](http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005). Original examples are written in *Scheme*, which is still a very popular *Lisp* dialect. Well, it is not so popular as *Clojure* these days, so I would like to convert prepared examples to that language instead.

At the beginning, I would like to focus on one thing - I will tell you, why this book is different, and why you should care about it. There are plenty books out there, which are doing a fairly well introduction to the programming, why do we even care about another one? Especially, if you are more than a beginner. *You should care because of two things*.

As every book related with teaching programming from the scratch, it introduces you to the topic from the ground up. It starts from the most basic concepts related with programming. Actually, if you are an experienced programmer, you will be disappointed by a first few dozens of pages. It will not introduce anything new, however it is written in a very interesting way - during the process you may think *"Well, at least it feels interesting when I am reading it"*. After first 50 pages, you will start seeing the first benefits of that book - it introduces you to the topic from the side of functional programming. In this book, assignment operator is introduced around 250 page. Every single concept introduced before that (e.g. various data structures, interesting tasks and algorithms) is literally pure (*free from shared state and side effects*) and immutable. Why this is an important thing? *Well, now I would like to introduce a second reason*.

[Free lunch is over](http://www.gotw.ca/publications/concurrency-ddj.htm). Last few decades we surf on top of huge wave directed by *Moore's Law*. Computer chips are getting smaller, faster and more effective in terms of power usage. But everything has its limits. Laws of physics are inexorable. We cannot bend the basic laws of nature, so instead making chips faster and smaller, we have to make *more* of them. And when I say *more*, I mean dozens, not only couple of them. In that case, not every responsibility can be dropped on the operating system and many of our programs should have notion of a *parallel execution* inside. But too many things related with *software engineering* today - programming languages, paradigms, solutions - are oriented around mutable and shared state.

![Amdahl's Law - Graph](http://upload.wikimedia.org/wikipedia/commons/e/ea/AmdahlsLaw.svg)

Metaphorically, we may say that instead of getting profits from *Moore's Law*, we have to deal with *Amdahl's Law* now. And that one of the basics truths inside computer science is also inexorable. It says that the maximum speedup in parallel environment is strictly related with its sequential part. Obviously, some parts have to be sequential, because of the inherent complexity related with the problem domain, however putting more and more constraints (related with mutability and shared state) increases this factor significantly. And in that case our parallel environment will be used inefficiently, or even worse - we are blocking resources which can be utilized in other parts of the system.

*Functional programming* exceeds your toolbox and enhances your programming skills, by bending them and enforcing to use them in a different way. By adding value to your toolbox, it introduces many powerful elements such as *referential transparency*, *mathematical conciseness*, *immutability*, *composability*, designing and thinking about parts of the system which are *pure* (side-effect free) and *impure* (abstracting side-effects away into different constructs like *Monads*). Those benefits are key factors when we have to deal with a concurrency and parallel environment - it is just easier to manage complexity in that way. Certain classes of problems, also related with concurrency, will disappear after choosing proper tool. Inferring the execution flow in such languages is also easier, because it is transparent (*no side effects*). 

I would like to introduce some amazing concepts from this book by emphasizing them in a series of articles posted on the blog. Starting from this one, I will speak about each chapter separately, mentioning important concepts, documented with a code samples written in *Clojure*. I will not focus on documenting things from the beginners point of view, but rather looking for analogies which an experienced programmer should understand. Let's start!

### Example

I would like to introduce to you a set of examples which will be a base for our discussion:

{% highlight clojure lineos %}
;; Recurrency is a powerful concept, which helps us traverse
;; collections. We can also abstract the actual transformation,
;; (which is represented as `term` here) and the evaluation
;; of the next step (represented as `next`).

(defn sum [ term a next b ]
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

;; Constructing new functions from other functions.
;; Remember, that operators are actually functions.

(defn cube [ n ]
  (* n n n))

(defn incr [ n ]
  (+ n 1))

;; Reusing the existing function inside the transformation.
;; Please, look that we describe what should be calculated,
;; not how it should iterate through range.

(defn sum-cubes [ a b ]
  (sum cube a incr b))

(println (sum-cubes 1 10))

;; Private functions can be created inside the scope.
;; Thanks to that we can hide unnecessary details from
;; the others.

(defn pi-sum [ a b ]
  (letfn [ (pi-term [ x ] (/ 1.0 (* x (+ x 2))))
           (pi-next [ x ] (+ x 4)) ]
    (sum pi-term a pi-next b)))

(println (* 8 (pi-sum 1 1000)))

;; Please look at the `add-dx` function - in that case
;; we are using function arguments inside from the higher
;; scope.

(defn integral [ f a b dx ]
  (letfn [ (add-dx [ x ] (+ x dx)) ]
    (* (sum f (+ a (/ dx 2.0)) add-dx b) dx)))

(println (integral cube 0 1 0.001))
{% endhighlight %}

#### Function as a *first-class citizen*

It is not a new concept. Literally every single mainstream programming language these days introduced the concept of *lambdas* (even Java :wink:). And prerequisite for that concept is having a notion of functions which are the *first-class citizens* in the language. It means that they can be passed around via variables and arguments, they can be returned and thanks to that - *they can be built* almost in a dynamic manner.

I you have experience and background in the *OOP*, you often worked with interfaces, which define only the protocol how rest of the code is using that specific part. If you worked with *C*, you used *function pointers* - it is also a definition of the protocol. Those concepts are the poor replacement of aforementioned mechanism.

#### *Just-enough* abstraction

Function is a *smallest possible abstraction*. Everything is transparent - it should have meaningful name, it has input arguments and it returns a result. Function will not mutate any global state, it will not mutate its arguments either. This means that it will not propagate any change outside of function. In the pure functional programming languages it cannot have even a single side-effect (e.g. using *I/O*). If we add that to the previous point - we have almost infinite possibilities to build and modify our *just-enough abstraction*, which will be safe and clean. Constructs like currying, recurrency, partial application will be our daily tool (we will talk about them in later posts).

I would like to point out one more thing. Many abstractions described as *a design patterns* can be represented as a such *just-enough abstractions* - e.g. decorator, facade, adapter. All of them can be represented as a single function.

#### Gentle start into *composability*

Composition is a very powerful mechanism. It is as a primary way to create new behaviours from the others. We can defer the execution by constructing the pipeline - it is a definition of *lazy computation*. Also composition is often useful, when we have to deal with transformations. In that case we are focusing on building the algorithm in a declarative way, modelling the purpose of it (*what should be done or calculated*) instead of describing every single bit and piece (*how it should be done*).

### *Interesting Examples*

At the end I would like to present the power of conciseness:

{% highlight clojure lineos %}
(defn first-denomization [ kinds-of-coins ]
  (condp = kinds-of-coins
    1 1
    2 5
    3 10
    4 25
    5 50))

(defn cc [ amount kinds-of-coins ]
  (cond (= amount 0) 1 
        (or (< amount 0) (= kinds-of-coins 0)) 0
        :else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomization kinds-of-coins))
                     kinds-of-coins))))

(defn count-change [ amount ]
  (cc amount 5))

(println (count-change 100))
{% endhighlight %}

That algorithm calculates how many combinations of coin exchanges we can have from a given amount. Thanks to the recurrency and splitting responsibilities into small functions, code is very readable and self explanatory.

### Credits

- [Amdahl's Law](http://upload.wikimedia.org/wikipedia/commons/e/ea/AmdahlsLaw.svg)
- [Structure and Interpretation of Computer Programs](http://mitpress.mit.edu/books/structure-and-interpretation-computer-programs), *Harold Abelson*, *Gerald Jay Sussman* and *Julie Sussman*
- [Full book available online](https://mitpress.mit.edu/sicp/full-text/book/book.html)
