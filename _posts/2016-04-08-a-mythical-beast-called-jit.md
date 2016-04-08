---
layout: post
title: A mythical beast called JIT
date: 2016-04-08T19:45+0200
tags:
  - v8
  - javascript
  - node.js
  - jit
---

# A mythical beast called `JIT`

I have recently bumped into one of those articles which title sounds rather like ["one weird trick"](https://top.fse.guru/nodejs-a-quick-optimization-advice-7353b820c92e) ad. Guiding just from it, you can imagine how small amount of information is contained there, and by actually going through that link there you will definitely make sure that it has some knowledge, but no real explanations are in place.

Of course, a final explanation is indeed really simple - `Function.prototype.toString()` returns everything even the comments inside the function body, size of that string is a feature that allows the optimizing compiler inside *V8* to make a decision to inline that particular function or not. There is even a command line switch which to use to modify a limit and a default value. Simple enough? Not really.

That article left a lot of unanswered questions. Probably because of that *JIT* compilers and its optimization techniques are kind of black magic.

## Why is it hard?

Obviously compiler engineers are not dumb people (they are actually really smart) and they are not obfuscating and complicating this by accident or on purpose. They are working hard, especially with such weakly typed and underspecified languages like *JavaScript* to provide you an optimized version of your code. Why is it a hard job? *It is all about guarantees*.

How many of them you can recall from memory when it comes to *JavaScript*? Not a lot of them, right? That is not good, especially from the perspective of a compiler engineer. *JavaScript* is everywhere, success of the internet spread this language, and nowadays success of *Node.js* pushed this language from front-end, even to server-side - and everywhere people are talking about performance. Obviously not about native like performance for scientific computations, but the more complexity is pushed to the application layer, the more of it will have to be optimized after all. That's why compilers and their creators have to be smart people - they have to deal with complexity of your applications and "*illness*" of the language itself.

Returning to the main topic, one of many tricks that they are using, which is used also in aforementioned case, is called *compilation cache*.

## V8, compilation cache and inlining

It is a really simple trick (do not confuse it with *inline caches* - it is a different technique!), where compiler textually compares the implementation of the function and if it is the same as the other one, merges it together and uses since then one representation. Why? Because if it is a hard process, why compiler should do it many times for the same code sample? If we can avoid it, we can speed-up the execution even more.

Second thing which is using *compilation cache* is *inlining*, where instead of function call (which can be expensive) we are putting directly small piece of code. The more similarities we detect, the better cache utilization will be. And of course we can free program related memory (or rather reuse it inside *V8* to other things).

But there are a culprit related to those techniques - it is related with the *textual representation* of function body. As in the original example, if the code differs by even a small piece that is inside a comment, it will be treated as a different thing - and in consequence *compilation cache* and *inlining* cannot be used properly. As a side note - regarding those two tricks compilers often are not working directly on the source code level, but at first they are transforming it to an *AST* or other more friendlier data structure for them. But not in case of *V8* - probably because of additional checks and computation that needs to be done, so authors relied on simple string comparison.

There is one additional trick that we can use, that is really well described (and related to both optimization techniques) by one of my favorite presenters and compiler evangelists *Vyacheslav Egorov* - he is using *UUIDs* hard-coded inside a comment in the function body to distinguish logically different functions, that could be potentially compiled and inlined from a single cache entry.

Why he would like to avoid that? In order to minimize the cache rebuilding and cache misses after recompilation and further passes of *JIT* compilation, when we're generating code or do other *fancy stuff* (if you do that *fancy stuff*, you are probably aware of how that things work in the first place).

Yeah, *JIT* and compilation techniques are *hard*. :wink:

## Credits

- [Node.js: A quick optimization advice](https://top.fse.guru/nodejs-a-quick-optimization-advice-7353b820c92e)
- [Vyacheslav Egorov: invokedynamic.js - JSConf EU 2014](https://youtu.be/YOHBZactXus?t=515)
- [Brilliant checklist for optimization in Node.js](http://mrale.ph/blog/2011/12/18/v8-optimization-checklist.html)
