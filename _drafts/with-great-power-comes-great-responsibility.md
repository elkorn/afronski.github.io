---
layout: post
title: With great power, comes great responsibility
---

# With great power, comes great responsibility

If you had to choose what is better - *short and concise code* or *abstractions visible at the first glance* - what would you choose?

Got your answer? Fine, let's look at the examples.

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

*Iterable* is an abstraction for all collections which can be stepped through one-by-one.

*Iterator* is a data structure (again, an abstraction) that allows to iterate over a sequence of elements. It has two methods - first for checking if there is a next element available, and second which returns the next element.

So far, so good - we can explicitly see an intent and usage for such abstractions. It is readable at the first sight.

# Observables

Things can get complicated if we are dealing with asynchronous operations, which return a collection as well. *Iterables* define only a collection of existing elements, not possible results.

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

We can compare these two concepts and definitely see similarities, such as that *Observable* is similar to *Iterable*. The same for *Observer* - *Iterator* pair. Also, we can easily spot an intent and usage here.

But when we add a new *trait* called *Subscription*, there is a slight and subtle change in approach for *Observable*.

It receives an *Observer* instance as an argument, instead of returning it as presented in the *Iterable* / *Iterator* approach.

This subtle change has huge consequences in modelling and understanding these two abstractions. We trade the possibility of pulling things out from an *Iterable* collection to pushing a model for the *Observable* which will notify an *Observer* that something appears, but not for free - we receive an ability to model an asynchronous operation which returns a collection of elements (in other words: we transformed from *pulling mode* to the *pushing mode*).

# Power of expressiveness

What if this *Observable* / *Iterable* approach was to be presented in one line as a composite type?

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

*`Try`* is an analog for the `try...catch` block. Instead of having an exception thrown and having to deal with it immediately in the same thread, it disconnects the error handling and recovery.

## `Option[T]`

{% highlight scala linenos %}
val optionResult = Option(null)   // optionResult is None.
val optionResult2 = Option(1)     // optionResult2 is Some(1).
{% endhighlight %}

*`Option`* represents optional values, returning an instance of *`Some(A)`* if `A` exists, or *`None`* if it does not.

## Composition

The same abstraction, of *Observable* can be built on top of *Iterable* and a simple type, which is a composite of two previously explained elements:

{% highlight scala %}
val collection: Iterable[Try[Option[T]]]
{% endhighlight %}

It's a sequence which introduces the same abstraction, but definitely loses the intent. Usage is not visible at the first glance as well (how are we communicating that something changed?).

# Summary

The whole point of this article is to illustrate that intent and usage can be provided by certain abstractions. Of course, these abstractions can be verbose and seem unnecessary at first sight, but if we chase expressiveness, and introduce it out of hand we will loose details and initial intent.

The post is strongly inspired by *Coursera* course *"Principles of Reactive Programming"* provided by courtesy of *Erik Meijer*, *Martin Odersky* and *Roland Kuhn*.

# References

1. [Coursera - Principles of Reactive Programming](https://www.coursera.org/course/reactive)