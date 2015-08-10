---
layout: post
title: Why every programmer should play with LEGO?
date: 2013-10-06T17:30+0200
tags:
  - LEGO
  - programming-practice
---

# Why every programmer should play with LEGO?

![Colorful LEGO pieces](/assets/LegoColorBricks.jpg)

## Disclaimer

I would like to strongly deny that the post has anything in common with a recent visit from my parents who brought me the entire supply of bricks. I won't say anything more without my lawyer :grin:.

## What does Christiansen's toy have in common with software engineering?

At a first glance, not much. But after a moment of playing you'll begin to notice some similarities. The most obvious one is the process of creating a new thing, even if software exist only in the computer's memory. The second similarity is related strictly to creativity, because playing with LEGO and building software are the same creative processes (with high proportion of imagination). But in my opinion there are more similarities, not so evident at the beginning.

### Standardization and Interfaces

When you're playing with bricks, it's easy to create new things, connect previously assembled elements with each other and simulate some missing parts, or even replace completely.

It's a programmer's dream: everything what is designed right now, fits perfectly to other parts assembled in past. Each component has elements which can be connected with other components in future. Common missing elements can be completely replaced or reassembled from smaller pieces.

By providing simple standardization and well defined interfaces (I guess the name *contract* is more appropriate in this case) you'll achieve amazing level of flexibility. Of course more specific elements (e.g. wheel and axle) aren't so flexible and interchangeable as the common bricks, but it's a balance between common use cases and more specified functionality. But, even if you have an old fashioned wheel from a stagecoach and a new one with a rubber tire, you can use both with the same axle.

### Modularity

Lets assume that you want to create a big castle like below:

![Big LEGO castle](/assets/BigLegoCastle.jpg)

At first you're creating walls, then towers with guards, a castle gate with a bascule bridge and in the meantime you'll fill it with king's headquarters and stables. When you'll finish the castle, you'll build the enemy armies with catapults (and finally destroy everything, because it's the most fun part :grin:).

Stand back and look how the process flows - you're creating modularized parts of the castle, then modularized elements of the enemy army. All modules can be connected together, each module has a single, well defined responsibility and you can easily join seemingly unrelated components together (because you can put enemy catapults inside your castle, creating kind of an alliance :wink:).

### DRY and Design Patterns

Yes, the *don't repeat yourself* rule have significant meaning when you're playing with LEGO, but it's formulated differently than the original one. In my opinion you're not reusing the assembled part itself (because you've got it in just one copy), but the way of creating it (in other words: you're reusing the way of assembling a certain thing).

Also the way of building a specific part can be called a *design pattern*, which can be easily reused in the future (when you have enough bricks and time you can repeat the process over and over again). In the example mentioned above you'll have a design pattern for your tower, but you can reapply it as many times to as many towers you need (you can, of course, adjust the previous design with some small enhancements, you aren't obligated to fulfill all the requirements and perform all the steps from previous case).

## Okay, but what if I don't have any LEGO bricks?

Then maybe, you're seeing similarities in your childhood toys when you're programming and solving problems. Or maybe you're seeing completely different analogies when you'll recall memories related with LEGO bricks. In both cases feel free to share your findings in the comments.

## Image credits

1. [Lego Color Bricks](http://en.wikipedia.org/wiki/File:Lego_Color_Bricks.jpg) (licensed on [CC BY-SA 2.0](http://creativecommons.org/licenses/by-sa/2.0/deed.en))
2. [Lego Castle Photo](http://www.flickr.com/photos/billward/3393269071/) (licensed on [CC BY 2.0](http://creativecommons.org/licenses/by/2.0/deed.en))
