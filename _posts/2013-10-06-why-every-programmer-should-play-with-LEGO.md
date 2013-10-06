---
layout: post
title: Why every programmer should play with LEGO?
---

# Why every programmer should play with LEGO?

![Colorful LEGO pieces](/assets/LegoColorBricks.jpg)

## Disclaimer

I would like to strongly deny that the post has anything in common with a recent visit of my parents who brought me the entire supply of bricks. I won't say anything more without my lawyer :grin:.

## What Christiansen's toy has in common with software engineering?

At first glance, not much. But after a moment of play and you'll begin to notice more similarities. The most obvious similarity is the process of creating new thing, even if software exist only in computer's memory. Second similarity is related strictly to the creativity, because playing with LEGO and building software are the same creative processes (with high proportion of imagination). But in my opinion there are more similarities, not so evident at the beginning.

### Standardization and Interfaces

When you're playing with bricks, it's easy to create new things, connect previously assembled elements with each other and simulate or even replace completely some missing parts.

It's a programmer's dream: everything what is designed right now, fits perfectly to other parts assembled in past. Each component has elements which can be connected with other components in future. Common missing elements can be completely replaced or reassembled from smaller pieces.

By providing simple standardization and providing well defined interfaces (I guess the name *contract* ) you'll achieve amazing level of flexibility. Of course more specific elements (e.g. wheel and axle) aren't so flexible and interchangeable as the common bricks, but it's a balance between common use cases and more specified functionality. But, even if you have a old fashioned wheel from a stagecoach and new wheel with rubber tire, you can use both with the same axle.

### Modularity

Lets assume that you want to create a big castle like below:

![Big LEGO castle](/assets/BigLegoCastle.jpg)

At first you're creating walls, than towers with guards, castle gate with bascule bridge and in the meantime you'll fill it with king's headquarters and stables. When you'll finish castle you'll build the enemy armies with catapults (and finally destroy everything, because it's the most fun part :grin:).

Stand back and look how the process flows - you're creating modularized parts of castle, than modularized elements of enemy army. All modules can be connected together, each module have single, well defined responsibility and you can easily attach even not related components with themselves (because you can put enemy catapults inside your castle, creating a kind of alliance :wink:).

### DRY and Design Patterns

Yes, *don't repeat yourself* rule have significant meaning when you're playing with LEGO, but it's formulated differently than original one. In my opinion you're reusing not the assembled part itself (because you've got it in just one copy), but the way of creating it (in other words: you're reusing the way of assembling certain thing).

Also the way of building a specific part can be called a *design pattern*, which can be easily reused in future (when you've enough bricks and time you can repeat the process over and over again). In example mentioned above you'll have design pattern for tower, but you can reapply it as many times as many towers you need (of course you can adjust previous design with some small enhancements, you aren't obligated to fulfil all requirements and perform all steps from previous case).

## Okay, but what if I don't have any LEGO bricks?

Than maybe, you're seeing similarities in your childhood toys when you're programming and solving problems. Or maybe you're seeing completely different analogies when you'll recall memories related with LEGO bricks. In both cases feel free to share your findings in the comments.

## Image credits

1. [Lego Color Bricks](http://en.wikipedia.org/wiki/File:Lego_Color_Bricks.jpg) (licensed on [CC BY-SA 2.0](http://creativecommons.org/licenses/by-sa/2.0/deed.en))
2. [Lego Castle Photo](http://www.flickr.com/photos/billward/3393269071/) (licensed on [CC BY 2.0](http://creativecommons.org/licenses/by/2.0/deed.en))