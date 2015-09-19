---
layout: post
title: SICP in Clojure - Chapter 3
date: 2015-09-18T16:00+0200
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

In the [previous blog post](http://www.afronski.pl/sicp-in-clojure/2015/08/07/sicp-in-clojure-chapter-2.html) (which had surprisingly good reception [on the Hacker News](https://news.ycombinator.com/item?id=10038515)) we have analyzed second chapter of the book. We did not know, that around the corner there is a ground breaking *"twist"* prepared by the authors in the 3rd chapter. We will face it today.

I should also mention that recently [SICP Distilled](http://www.sicpdistilled.com/) went publicly, which is also a very good tour on the book, with a different approach. Nevertheless, I am still going my way through the book and I have got couple of interesting observations from the aforementioned chapter. But first, I would like to address one question which was posted on the *Hacker News* thread.

### Clojure is not a Scheme and `SICP` will not help you to learn Clojure

Even if the first part is not debatable at all (how come one language could be identical to another one, even if they have common roots), then the second one is definitely dependent on the point of view. Mine is really obvious - *Clojure* is my first contact with a *Lisp-like* language. And book has much more to offer than a simple language course. I would say that a language course and SICP can be complementary. How?

Basically (from my observations) aforementioned book is about *good software design* and how to approach *functional programming* and *functional design* in a *dynamicly typed language*. It attacks various problems like *modularity*, *data structures*, and many more. It provides a good foundation for beginners, and I strongly believe that it can be beneficial for more advanced programmers. By taking that book and completely new language, I am treating this book more like a reference and skeleton - which can be filled when I play with the new language. Of course, it is not a tutorial how to approach a language correctly, in an idiomatic way. Instead, I am building references, analogies and comparisons, which opens a different mode in my head than working with the new language solely. Also, I can focus on the learning by comparison - but I need to be careful, because instead learning new thing, e.g. I can mirror constructs in *Clojure* which are natural only for *Scheme*.

Probably because of that someone brought Rich Hickey's opinion about `SICP` to the table:

<quote class="citation">I personally don't think SICP will help you much with Clojure. YMMV.</quote>

And it is hard to argue with that as well - it is *true*, SICP has nothing to do with learning *Clojure*. But I can refer to the topics which I already know and thanks to that - learn Clojure at the same time. It is beneficial that I can seek by myself for an idiomatic path to describe the same thing, but in *a Clojure way*. Of course, there will be plenty of mistakes in the process - but hey, isn't that the most effective way to learn? :wink:

### Danger is hiding around the corner

Returning to the main topic - we went more than 200 pages through the book, we built various data structures, simple and not so simple programs which solve certain problems. And around the 215th page (I am referring to *Polish* edition :wink:) authors introduced concept of *mutable state*. You may think *"oh come on, we are dealing with that on the daily basis"*. Why it is so dangerous? Let's look at the example:

{% highlight clojure linenos %}
(defn make-accumulator [start]
  (let [acc (atom start)]
    (fn [x]
      (swap! acc + x))))

(def A (make-accumulator 5))

(A 10) ;; 15
(A 10) ;; 25
{% endhighlight %}

If you do not understand what an `atom` is, do not worry - we will get [there](#mutability-in-clojure).

Now, I would like to refer to some features that functions without mutable state have. Till now, our functions were fully and only dependent on the input arguments. Taking those values, and body of the function you could clearly reason about the result. Output of that operation could be calculated with the simple substitution method. In other words - our programs were *referentially transparent* - we could substitute one part of the program with its calculated, simpler version and the result will be still the same. In the example presented above we are not passing state from the previous invocation explicitly - it is buried inside the *computation object* (in our case an *accumulator*).

By introducing concept of mutable state, our simple substitution methods are worthless. Functions are not only dependent on arguments, but also on the previous state (*local* or even worse - *global* one). We need to deal with the concept of *time* in our programs. Perfect, by that we just added one more variable to our environment. It means that, our simple invocation of function can cause a *side effect*. Taking the previous term - our programs became *referentially opaque*.

### What is a *side effect*?

It is an additional behavior (sometimes deliberate, often unwanted - either unaware or forgotten) in our environment related with the function invocation internals. It is either a mutation of some kind of state, or function invocation, which causes mutation. It means that *something* in our execution environment is different before and after the function call. Without analyzing body of the function, and its previous states we are not capable of defining what kind of change it is and where it happened. Pretty scary, huh? Well, welcome to the *imperative programming* world.

It does not mean that imperative programming is particularly bad, you can write pretty clean programs in those languages if you will stick to certain rules and guides. Moreover, all kind of *I/O* is a *side effect* - it does not mean that it is bad, but it changes the state - it mutates the environment. It has disadvantages and advantages (sometimes it is simpler to model things in that way - e.g. `random` numbers generator which relies on some mutable *seed* state). I should mention also that there are some languages which are really strict when it comes to the *I/O*, *side effects* and execution - one example is *Haskell* (described [here](http://www.afronski.pl/7-languages-in-7-weeks/2015/08/26/seven-languages-in-seven-weeks-haskell.html)). How they enforce strictness on that, is a topic for another blog post - *Clojure* is different. **It is pragmatic**.

Language philosophy encourages you to write pure, functional core (by that I mean that functions are *referentially transparent*, without *mutable state* and *side effects*). But, it allows you in a very controlled and easy way deal with all kind of *side effects* and *impure* peripherals. We will define them later.

### Another, horrible consequence of mutability

There is one more consequence of mutability. If we will think for a moment, by introducing *mutable state*, we introduced notions of *time* and *resource ownership* (someone is an owner of that mutable state, it does not flow from one call to another via input and output). It means that someone is an owner of a particular state, it can be changed there, but others can also read it. Or even worse - sometimes multiple actors can modify the state. In both cases *sharing* introduces some really nasty consequences, because it means that time and access to that resource need to be synchronized between multiple parties. **And, as you can imagine, that causes awful lot of problems**.

### Mutability in Clojure

When you approach *Clojure* for a first time (especially if you are approaching it with an experiences from *imperative programming* world), you may think that by creating a *global* or *local* `vars` via `def` it can be shared between multiple execution contexts. Thankfully, you cannot share them - all `vars` are isolated. It means that you cannot change it from a different execution context e.g. a *thread*. Changing state is possible only by rebinding it locally for that context, via `binding`:

{% highlight clojure linenos %}
(def ^:dynamic x 1)
(def ^:dynamic y 1)

(+ x y)             ;; 2

(binding [x 2 y 3]
  (+ x y))          ;; 5

(+ x y)             ;; 2
{% endhighlight %}

In other words `vars` ensure safe use of mutable storage locations via thread isolation. And one more remark - it reminds much more an *imperative style* of programming, and you have available more of those constructs like `with-local-vars` - but, it is not a recommended way to deal with problems in *Clojure*.

Probably you have heard that *Clojure* has *STM* (*Software Transactional Memory*, exact details about the implementation are gathered [here](http://clojure.org/refs)). And that is true, you can ensure shared use of mutable storage thanks to that. But you have to use a different concept - it is called a `ref`. They are bound to a single storage location through their lifetime, and allow only to mutate value in that location to happen only within a *transaction*. Sounds familiar, right? Let's look at the example:

{% highlight clojure linenos %}
(def pending-jobs (ref #{4 3 2}))
(def active-jobs (ref #{1}))
(def done-jobs (ref #{}))

(def start-job [n]
  (dosync
    (commute pending-jobs disj id)
    (commute active-jobs conj id)))

(def finish-job [n]
  (dosync
    (commute active-job disj id)
    (commute done-jobs conj id)))

@pending-jobs     ;; #{4 3 2}
@active-jobs      ;; #{1}
@done-jobs        ;; #{}

(finish-job 1)

@pending-jobs     ;; #{4 3 2}
@active-jobs      ;; #{}
@done-jobs        ;; #{1}

(start-job 2)

@pending-jobs     ;; #{4 3}
@active-jobs      ;; #{2}
@done-jobs        ;; #{1}
{% endhighlight %}

In other words - it is a *synchronous* and *synchronized* way of altering *shared* mutable state. Keep in mind that values placed inside a `ref` should be *immutable*. Otherwise something outside of transaction scope attached to a mutable storage can change values inside, and language will not help you in managing that part (in our example we have used plain and immutable *Clojure* data structure - *sets*).

In the first example attached in that blog post we have used an `atom` as a local state representation. It is an easy way to handle *shared* state in a *synchronous* and *independent* manner. It means that it is an ideal way of having an *internal*, *shared* state encapsulated somewhere in the *function closure*:

{% highlight clojure linenos %}
(defn make-monitored [f]
  (let [counter (atom 0)]
      (fn [arg]
        (condp = arg
          'reset-count (reset! counter 0)
          'how-many-calls? @counter
          (do (swap! counter inc) (f arg))))))

(def sqr (make-monitored (fn [x] (Math/sqrt x))))

(println (sqrt 100))
(println (sqrt 'how-many-calls?))
(println (sqrt 25))
(println (sqrt 'how-many-calls?))
(println (sqrt 'reset-count))
(println (sqrt 'how-many-calls?))
{% endhighlight %}

*Agents* are last option which *Clojure* has regarding the mutable storage mechanisms. They are different from the *atoms*, because state application is *asynchronous*. You can think about an `agent` as a sink, into which we are sending messages. They will be applied asynchronously in the order of receiving them. Let's look at the example:

{% highlight clojure linenos %}
(def log-sink (agent '()))

(defn debug [msg]
  (send log-sink conj (str "DEBUG: " msg)))

(defn info [msg]
  (send log-sink conj (str "INFO: " msg)))

;; Example presented here is of course simplified, but
;; imagine that multiple threads are executing those
;; logging statements - in that case, all of them will return
;; immediately, and changes will be applied in the order of
;; receiving them on the `agent` side.

(debug "1")
(debug "2")
(info "3")
(debug "4")

@log-sink     ;; ("DEBUG: 4" "INFO: 3" "DEBUG: 2" "DEBUG: 1")
{% endhighlight %}

We have talked about all options related with *mutability*, now it is time to check and verify them in practice. Authors of the book prepared something special for us.

### Designing system with mutability - Electronic Circuit Simulator

There is a very nice example presented in the book, which implements circuit board simulation. Authors implement it with use of mutable state, represented as an encapsulated *computation objects*. Let's look at part of that implementation (whole can be found [here](https://github.com/afronski/sicp-examples/blob/master/chapters/3/3.3.4/electronic-circuit.clj)):

{% highlight clojure linenos %}
;; Wires - one of the computational objects in the example.
;;
;; It represents a *signal state* and list of actions called *effects*
;; which are executed after the signal propagates through the wire.

(defn get-signal [wire] (wire :get-signal))
(defn set-signal! [wire v] ((wire :set-signal!) v))
(defn add-action! [wire p] ((wire :add-action!) p))

(defn make-wire []
  (let [signal (atom false)
        effects (atom (list))]
    (letfn [(set-signal! [new]
              (if (not (= signal new))
                (do (reset! signal new)
                    (invoke-all @effects))
                :done))

            (add-action! [procedure]
              (swap! effects conj procedure)
              (procedure))

            (dispatch [action]
              (condp = action
                :get-signal @signal
                :set-signal! set-signal!
                :add-action! add-action!
                (assert false (str "Unknown operation " action " in make-wire."))))]
      dispatch)))

;; ...

;; Adders - the functional composition of previously defined smaller elements, with
;; use of local computational objects with state, represented by wires.

(defn half-adder [a b s c]
  (let [d (make-wire)
        e (make-wire)]
    (or-gate a b d)
    (and-gate a b c)
    (not-gate c e)
    (and-gate d e s)
    :ok))

(defn full-adder [a b c-in sum c-out]
  (let [s (make-wire)
        c1 (make-wire)
        c2 (make-wire)]
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    :ok))

;; Simulation - the actual use of the system. Scheduling, gate propagation
;; delay and agenda are hidden underneath the `step` and `set-signal!` functions.

(def input-1 (make-wire))
(def input-2 (make-wire))
(def sum (make-wire))
(def carry (make-wire))

(probe :sum sum)
(probe :carry carry)

(half-adder input-1 input-2 sum carry)

(set-signal! input-1 true)
(step)

(set-signal! input-2 true)
(step)
{% endhighlight %}

Even if using the system, is really easy (last part is actually a very pleasant and simple *DSL*) - reasoning about the state is definitely harder. I encourage you to analyze the actual implementation (and how the *agenda* mechanism works).

### Laziness

Before we will move to the next chapter, authors introduced a *stream* concept, which is a lazy sequence. It means that only the first value is available and tail will be calculated on demand afterwards (or will not - depends on the further execution flow). In *Scheme* you have to build such thing by yourself, in *Clojure* - you have got all facilities in place already:

{% highlight clojure linenos %}
(defn integers-from [n]
  (cons n (lazy-seq (integers-from (inc n)))))

(def integers (integers-from 1))

(println (take 10 integers))
{% endhighlight %}

Keep in mind that we are using the same functions from the `Seq` interface (`first`, `rest` and `cons`) despite that the actual sequence is evaluated lazily. In more comprehensive [example](https://github.com/afronski/sicp-examples/blob/master/chapters/3/3.5.2/infinite-streams.clj) you can see how you can use filtering or mapping together with laziness.

### Using streams, or rather - why laziness is a good thing?

When you read previous section, you probably have a feeling that laziness introduces a better way for handling state. It optimizes certain use cases, because often we do not want to calculate and proceed operations on all elements. Indeed, that is a *better way* to approach problem of state succession - but, keep in mind that all performed operations that have side effects can be problematic. It means that some parts of the code would not be evaluated, so side effects will not be applied either - what if something, down in the guts of the system relies on that? That is another reason why mutation causes unexpected problems.

In the last section of the chapter authors compared modularity of the *functional design* and *objects*. They have prepared an [interesting example](https://github.com/afronski/sicp-examples/blob/master/chapters/3/3.5.5/monte-carlo.clj) which combines *computational object* and underneath uses *streams* (and in consequence, *lazy sequences*):

{% highlight clojure linenos %}
;; Monte Carlo method as a stream (you are increasing number
;; of iterations by taking more elements from the stream).

(defn monte-carlo [experiment-as-stream passed failed]
  (letfn [(next [passed failed]
            (cons (/ passed (+ passed failed))
                  (lazy-seq (monte-carlo (rest experiment-as-stream)
                                         passed
                                         failed))))]
    (if (first experiment-as-stream)
      (next (inc passed) failed)
      (next passed (inc failed)))))
{% endhighlight %}

It is an implementation of [Monte Carlo simulation method](https://en.wikipedia.org/wiki/Monte_Carlo_method). It is based on the streams - they are responsible for transformation and representing simulation itself, and has simple *computational object* which represents *random number generator* (with internal and *mutable* state).

### Summary

It was a *very long article*, with a lot of twists and a groundbreaking truth. For developers with some experience (especially related with *multi-threading* and *concurrency*) it is probably a bread and butter - that is why we are unconsciously got used to it. Change and reflection about state of our tools requires fresh point of view, a new perspective. I hope that by reading those kind of books I will easily get one. And thanks to that I will be able to rethink my daily practices, and obviously learn new stuff.

See you next time, in the blog post about 4th chapter! :wink:

### Credits

- [Structure and Interpretation of Computer Programs](http://mitpress.mit.edu/books/structure-and-interpretation-computer-programs), *Harold Abelson*, *Gerald Jay Sussman* and *Julie Sussman*
- [Full book available online](https://mitpress.mit.edu/sicp/full-text/book/book.html)
- [afronski/sicp-examples](https://github.com/afronski/sicp-examples)
- [SICP Distilled](http://www.sicpdistilled.com/)
