---
layout: post
title: Seven Languages in Seven Weeks - Io
date: 2015-04-30T16:00+0200
---

# Seven Languages in Seven Weeks - Io

<quote class="disclaimer">This blog post is a next article of a series related with books "Seven Languages in Seven Weeks" and its sequel. Each post will describe a single language chosen by this book and its most interesting and influencing feature, from my point of view and previous experiences. I hope that you will find this series interesting. Do not hesitate with sharing your feedback and comments below!</quote>

# Introduction

<img class="right io-logo" alt="Io Logo" src="/assets/IoLogo.png" />

At first you may think that blog post is about *io.js*, a recent *node.js* fork - but is not, we are talking here about programming languages. :wink:

*Io* is a programming language created by *Steve Dekorte* in 2002. It is described as second in the aforementioned book *Seven Languages in Seven Weeks*. The only thing that it has in common<br/>with *io.js* / *node.js* and other *JavaScript* based platforms is its prototypical nature.

It means that like in *Self* (or *JavaScript*) everything is a *clone* of another object and like in *Smalltalk* everything is an object. In other words - **there is no distinction between class and instance** and you can build *classes* and its schema during *run-time* execution.

{% highlight io linenos %}
Car := Object clone
//   Car_0xDEADBEE:
// type            = "Car"

Car drive := method("Vroom!" println)
// method(
//  "Vroom!" println
// )

Car drive
// Vroom!
{% endhighlight %}

As you can observe above - *syntax is pretty minimal*. It is often compared to the *Lisp-like* languages. Besides that, language has really clear semantics, which is easy to grasp, powerful features in the standard library (also related with the *concurrency* support) and small, portable *virtual machine* (which is often used in the *embedded systems* domain).

I would like to bring some light to the most interesting language features, starting from the basic things.

## Slots and messages

{% highlight io linenos %}
Car := Object clone
Car desc := "A simple car."

Car slotNames
// list("type", "desc")
{% endhighlight %}

## Prototype chains

{% highlight io linenos %}
Car := Object clone
Car desc := "A simple car."

Ferrari := Car clone
Ferrari type
// Ferrari

testarossa := Ferrari clone
testarossa type
// Ferrari

testarossa slotNames
// list()

testarossa desc
// "A simple car."
{% endhighlight %}

## Message passing

Everything is a message - even the method invocation is represented as a message passing.

## Nice examples

### Singleton

It is very easy to create a true *singleton* instance in the prototypical language. We just need to provide our implementation in the *clone* slot for that instance. Our implementation will return always the same copy. This method requires consistency in the language - it should be only one way to create an object copy.

{% highlight io linenos %}
Single := Object clone
Single clone := Single
{% endhighlight %}

In presented case, each clone invoked on the *Single* object will return always the same instance.

### Concurrency support

*TODO*: Futures, Coroutines and building an *Actor* from them.

### Extending interpreter and VM

And finally, most advanced but still concise example - related with extensiveness of the *language* and a *run-time*.

{% highlight io linenos %}
OperatorTable addAssignOperator(":", "atPutNumber")

curlyBrackets := method(
    r := Map clone
    call message arguments foreach(arg,
        r doMessage(arg)
    )
    r
)

Map atPutNumber := method(
    self atPut(
        call evalArgAt(0) asMutable removePrefix("\"") removeSuffix("\""),
        call evalArgAt(1)
    )
)

// Structure of `data.json` file:
//
// {
//    "key": 123456,
//    "another_key: 4567890
// }

content := File with("data.json") openForReading contents

data := doString(content)

data keys println
data values println
{% endhighlight %}

Of course, not a full *JSON* specification is implemented here, but thanks to the ability to overload operator meaning, creating new operators and modifying its precedence (all of this done by manipulating *OperatorTable*) we can extend our interpreter at *run-time*. Thanks to that extension, it will *meaningfully* evaluate data read, directly from the file contents.

### Summary

In the next blog post we will talk about another not popular, but powerful and *mind cracking* programming language from the *70s* - *[Prolog](http://www.learnprolognow.org/)*. It provides declarative beauty connected together with a logic nature. Different approach to computation expressed by *facts* and *rules* mingled together with *relations* can really bend your mind and guide you in the really strange, but also entertaining directions - like an annoying, but still interesting *puzzle*.

### Credits

- [Seven Languages in Seven Weeks](https://pragprog.com/book/btlang/seven-languages-in-seven-weeks), *Bruce A. Tate*
- [Steve Dekorte, Github](https://github.com/stevedekorte)
- [Io, programming language](http://iolanguage.org)
