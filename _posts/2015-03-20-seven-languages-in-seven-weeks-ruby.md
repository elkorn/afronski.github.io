---
layout: post
title: Seven Languages in Seven Weeks - Ruby
date: 2015-03-20T16:00+0100
---

# Seven Languages in Seven Weeks - Ruby

<quote class="disclaimer">This blog post is a starting point of a series related with books "Seven Languages in Seven Weeks" and its sequel. Each post will describe a single language chosen by this book and its most interesting and influencing feature, from my point of view and previous experiences. I hope that you will find this series interesting. Do not hesitate with sharing your feedback and comments below!</quote>

### Introduction

<img class="right ruby-logo" alt="Ruby Logo" src="/assets/RubyLogo.png" />

Aforementioned book has a very controversial concept called an *ugly child*. One of chosen languages is presented as a *necessary evil*, used in the old era. The choice fell on the *Ruby*. And I partially agree with that choice.

Do not get me wrong - *Ruby* is a wonderful language, it brings multiple merits and valuable tools to us. In many cases it rescued many programming careers from boredom and daily routine. It restored *happiness* to the job of many programmers. But in this book, this language is surrounded by many other languages - better ones, relatively to the era that is coming (or rather - which is already here).

It is an old language - created in 1995 (the same year that *Java* was created). This does not matter, when you have to get your job done, but it matters when it comes to the evolution, that took place since then. Author chosen that language because of joy and happiness that it brings for him. **<em>Ruby is optimized for developer happiness</em>**. Moreover, many tools that just get the job done are written in it (*Rails*, *Sinatra*, *Capistrano*, *Chef*, *Vagrant* and many, many more). They definitely influenced many other communities (try to count how many *Sinatra* forks are already created :wink:).

From the community itself we can also learn multiple things. Initiatives like [Rails Girls](http://railsgirls.com/), [Ruby Tapas](http://www.rubytapas.com/), [Exercism](http://exercism.io/), supporting diversity, embracing beginners and *greenhorns* in the community, examples that come from the top - from core contributors, people like [@yukihiro_matz](https://twitter.com/yukihiro_matz), [@josevalim](https://twitter.com/josevalim) or [@tenderlove](https://twitter.com/tenderlove) - that really makes a *change*.

Besides that, language itself have many interesting features that brings joy, but also enable nice use cases, hard to implement in other mainstream programming languages.

### Why this language?

I would like to present only a part of the most interesting feature - *method_missing* and ability to easily and efficiently use *metaprogramming* in your daily activities. It is a key thing that enables many use cases - starting from the crazy things, like that one presented below and ending on the various *Domain Specific Languages*.

{% highlight ruby linenos %}
class RomanNumerals
  def self.method_missing name, *args
    roman = name.to_s
    roman.gsub!("IV", "IIII")
    roman.gsub!("IX", "VIIII")
    roman.gsub!("XL", "XXXX")
    roman.gsub!("XC", "LXXXX")

    (roman.count("I") +
     roman.count("V") * 5 +
     roman.count("X") * 10 +
     roman.count("L") * 50 +
     roman.count("C") * 100)
  end
end
{% endhighlight %}

How it works? Lets look on the *`REPL`* output:

{% highlight ruby %}
irb(main):001:0> RomanNumerals.X
=> 10
irb(main):002:0> RomanNumerals.XCII
=> 92
irb(main):003:0> RomanNumerals.XII
=> 12
irb(main):004:0> RomanNumerals.XIV
=> 14
{% endhighlight%}

For each undefined method in that class, we are calling an entry point called *`method_missing`*. Then you can react and do whatever you want with the actual input arguments and invoked method name. This feature, connected with very flexible and liberal syntax, enables any kind of *DSL* creation that you can possibly imagine.

Why I have presented only this part? Because *Ruby* is already very popular programming language, I would say even a mainstream one. I do not want to focus on describing its features, instead we should learn how to build an *amazing community*.

This environment was possible to build thanks to that language. Besides the described feature, additional ones like *mixins*, *blocks* or very complete, cohesive and well-documented *standard library* really let you enjoy process of creating. Many people feel joy of programming when dealing with *Ruby*. This is a key point, because that feeling attracts creative people. And these people are the creators of very vibrant and active community, these people are creators of amazing tools and libraries. And sometimes they transfer their experiences to the other communities (like [@josevalim](https://twitter.com/josevalim) and *Elixir* programming language community).

**It is just simple as that**.

### Summary

In the next blog post we will talk about not so popular, but still very elegant and interesting prototype-based language described in *"Seven Languages in Seven Weeks"* book as a second one  - the [Io](http://iolanguage.org) programming language.

### Credits

- [Seven Languages in Seven Weeks](https://pragprog.com/book/btlang/seven-languages-in-seven-weeks), *Bruce A. Tate*
