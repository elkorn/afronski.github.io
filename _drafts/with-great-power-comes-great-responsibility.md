---
layout: post
title: With great power, comes great responsibility
---

# With great power, comes great responsibility

If you have to choose about what is better - *short and concise code* or *abstractions visible at the first glance* - what will you choose?

You have got your answer? Fine, let's look at the examples.

# Iterables

Let's look at the standard abstraction of *iterable collections*, available in Java and Scala (example written in *Scala*):

{% highlight scala linenos %}
trait Iterable[T] {
  def iterator(): Iterator[T]
}

trait Iterator[T] {
  def hasNext: Boolean
  def next(): T
}
{% endhighlight%}

*Iterable* is an abstraction for all collections which can be step through one-by-one.

*Iterator* is a data structure (again, an abstraction) that allow to iterate over a sequence of elements. It has two methods - first for checking if there is a next element available, and second which returns the next element.

So far, so good - we can explicitly see an intent and usage for such abstractions. It is readable at the first sight.

# Observables

Things can complicate if we deal with asynchronous operations, which returns a collection as well. *Iterables* are defining collection of existing elements, not possible results.

But we can introduce new abstractions. Let's look at the example (again, written in *Scala*):

{% highlight scala linenos %}
trait Observable[T] {
  def subscribe(observer: Observer[T]): Subscription
}

trait Observer[T] {
  def onNext(value: T): Unit
  def onError(error: Throwable): Unit
  def onCompleted(): Unit
}

trait Subscription {
  def unsubscribe(): Unit
}
{% endhighlight %}

We can compare these two concepts and definitely see similarities, like *Observable* is similar to *Iterable*, the same for *Observer* - *Iterator* pair. Also we can easily spot here an intent and usage.

But we add new *trait* called *Subscription* and there is a slight and subtle change in approach for *Observable*.

It receives as an argument the *Observer* instance, instead of returning it as it is presented in *Iterable* / *Iterator* approach.

This subtle change has huge consequences in modeling and understanding these two abstractions - we trade possibility of pulling things out from *Iterable* collection to pushing model for the *Observable* which will notify and *Observer* that something appears, but not for free - we receive an ability to model asynchronous operation which returns a collection of elements (in other words: we transformed from *pulling model* to the *pushing mode*).

# Power of expressiveness

What if I will present this *Observable* / *Iterable* approach in one line as a composite type?

Ready? Superb, but first we will bring up two definitions.

## `Try[T]`

{% highlight scala linenos %}
val sumTry = for {
  int1 <- Try(Integer.parseInt("1"))   // int1 is Success(1).
  int2 <- Try(Integer.parseInt("a"))   // int2 is Failure(...).
} yield {
  int1 + int2
}
{% endhighlight %}

*`Try`* is an analogue for the `try...catch` block. Instead of having an exception thrown and having to deal with it immediately in the same thread, it disconnects the error handling and recovery.

## `Option[T]`

{% highlight scala linenos %}
val optionResult = Option(null)   // optionResult is None.
val optionResult2 = Option(1)     // optionResult2 is Some(1).
{% endhighlight %}

*`Option`* represents optional values, returning an instance of *`Some(A)`* if `A` exists, or *`None`* if it does not.

## Composition

The same abstraction, of *Observable* can be built on top of *Iterable* and simple type which is a composite of two, previously explained elements:

{% highlight scala %}
val collection: Iterable[Try[Option[T]]]
{% endhighlight %}

It's a sequence which introduces the same abstraction, but definitely it lost the intent and also usage is not visible at the first glance (how we are communicating that something changed?).

# Summary

The whole point of this article is to illustrate, that intent and usage can be provided by certain abstractions. Of course, these abstractions can be verbose and seems unnecessary at first sight, but if we will chase expressiveness, and introduce it out of hand we will loose details and initial intent.

Post is strongly inspired by *Coursera* course *"Principles of Reactive Programming"* provided by courtesy of *Erik Meijer*, *Martin Odersky* and *Roland Kuhn*.

# References

1. [Coursera - Principles of Reactive Programming](https://www.coursera.org/course/reactive)