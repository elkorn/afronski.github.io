---
layout: post
title: SICP in Clojure - Chapter 1
date: 2015-06-04T16:00+0200
---

# SICP in Clojure - Chapter 1

### What is a *SICP* and why should you care?

<quote class="disclaimer">In on of the <a href="http://www.afronski.pl/2015/06/01/books-that-changed-my-career-structure-and-interpretation-of-computer-programs.html">previous blog post</a> I have announced that I would like to start today a new series of posts. It is a persistent journal from my journey through this book. I hope that you will enjoy it and find it useful</quote>

*SICP* is an acronym which represents a classic, computer science book and stands for *"Structure and Interpretation of Computer Programs"*. It is written by Harold Abelson, Gerald Jay Sussman and Julie Sussman. It is a base book for an introductory course in programming on MIT. Original examples are written in *Scheme*, which is a very popular *Lisp* dialect. Well, it is not so popular as Clojure these days, so I would like to convert prepared examples to that language instead.

At the beginning, I would like to focus on one thing - I would like to tell you, why this book is different, and why you should care. There are plenty books out there, which are doing a fairly well introduction to the programming, why do we even care about another one? *Because of two things*.

As every book related with teaching programming from the scratch, it introduces you to the topic from the ground up. It starts from the most basic concepts related with programming. Actually, if you are an experienced programmer, you will be disappointed by a first few dozens of pages. It will not introduce anything new, however it is written in very interesting way - you may think "Well, at least it feels interesting when I am reading it". After first 50 pages, you will start seeing the first benefits of that book - it introduces you to the topic from the side of functional programming. Why this is an important thing? *Well, now I would like to introduce second reason*.

*Free lunch is over*. Last few decades we surf on top of huge wave directed by *Moore's Law*. Computer chips are getting smaller, faster and more effective in terms of power efficiency. But everything has its limits. Laws of physics are inexorable. We cannot bend the basic laws of nature, so instead making chips faster and smaller, we have to make *more* of them. And when I say *more*, I mean dozens, not only couple of them. In that case, not every responsibility can be dropped on the operating system and many of our programs should have notion of a *parallel execution* inside. But too many things related with *software engineering* today - programming languages, paradigms, solutions are oriented around mutable and shared state.

![Amdahl's Law - Graph](http://upload.wikimedia.org/wikipedia/commons/e/ea/AmdahlsLaw.svg)

Metaphorically, we may say that instead of getting profits from *Moore's Law*, we have to deal with *Amdahl's Law* now. And that one of the basics truths inside computer science is also inexorable. It says that the maximum speedup in parallel environment is strictly related with its sequential part. Obviously, some parts have to be sequential, because of the inherent complexity related with the problem domain, however putting more and more constraints (related with mutability and shared state) increases this factor significantly. And in that case our parallel environment will be used inefficiently, or even worse - we are blocking resources which can be utilized in other parts of the system.

*Functional programming* exceeds your toolbox and enhances your programming skills, by bending them and enforcing to use them in a different way. By adding value to your toolbox, it introduces many powerful elements such as *referential transparency*, *mathematical conciseness*, *immutability*, *composability*, designing and thinking about parts of the system which are *pure* (side-effect free) and *impure* (abstracting side-effects away into different constructs like *Monads*). Those benefits are key factors when we have to deal with concurrency and parallel environment - it is just easier to manage complexity in that way. Certain classes of problems will disappear after choosing proper tool, inferring the execution flow in such languages is also easier, because it is transparent. 

I would like to introduce some amazing concepts from this book by emphasising them in a series of articles posted on the blog. Starting from this one, I will speak about each chapter separately, mentioning important concepts, documented with a code samples written in *Clojure*. I will not focus on documenting things from the beginners point of view, but rather looking for analogies which an experienced programmer should understand. Let's start!

### Example

{% highlight clojure lineos %}
(defn sum [ term a next b ]
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(defn cube [ n ]
  (* n n n))

(defn incr [ n ]
  (+ n 1))

(defn sum-cubes [ a b ]
  (sum cube a incr b))

(println (sum-cubes 1 10))

(defn pi-sum [ a b ]
  (defn pi-term [ x ]
    (/ 1.0 (* x (+ x 2))))
  (defn pi-next [ x ]
    (+ x 4))
  (sum pi-term a pi-next b))

(println (* 8 (pi-sum 1 1000)))

(defn integral [ f a b dx ]
  (defn add-dx [ x ] (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(println (integral cube 0 1 0.001))
{% endhighlight %}

#### Function as a *first-class citizen*

*TODO*: passing functions around, returning them.

#### *Just-enough* abstraction

*TODO*: Smallest possible abstraction. Everything is transparent - it has name, codified algorithm, input and output. Currying, recurrency, partial application are your friends - many abstractions described as a design patterns can be represented as a functions - e.g. decorator, facade.

#### Gentle start into *composability*

*TODO*: composition as a primary way to create new functions.

### *Interesting Examples*

{% highlight clojure lineos %}
(defn first-denomization [ kinds-of-coins ]
  (cond (= kinds-of-coins 1) 1
        (= kinds-of-coins 2) 5
        (= kinds-of-coins 3) 10
        (= kinds-of-coins 4) 25
        (= kinds-of-coins 5) 50))

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

### Credits

- [Amdahl's Law](http://upload.wikimedia.org/wikipedia/commons/e/ea/AmdahlsLaw.svg)
- [Structure and Interpretation of Computer Programs](http://mitpress.mit.edu/books/structure-and-interpretation-computer-programs), *Harold Abelson*, *Gerald Jay Sussman* and *Julie Sussman*
- [Full book available online](https://mitpress.mit.edu/sicp/full-text/book/book.html)
