---
layout: post
title: Seven Languages in Seven Weeks - Haskell
date: 2015-08-26T16:30+0200
categories:
  - 7-languages-in-7-weeks
tags:
  - series
  - 7-languages-in-7-weeks
  - programming-languages
  - haskell
  - books
---

# Seven Languages in Seven Weeks - Haskell

<quote class="disclaimer">This blog post is a next article from series related with books "Seven Languages in Seven Weeks" and its sequel. Each post will describe a single language chosen by this book and its most interesting and influencing features from my point of view and previous experiences. I hope that you will find this series interesting. Do not hesitate with sharing your feedback and comments below!</quote>

### Introduction

I was aware of *Haskell* existence a long time ago (around 2009 or 2010). At that time, I thought that it is a *purely academical programming language* with no actual industrial usage. **I could not be more wrong**.

My first actual experience, when I rediscovered *Haskell*, was surprisingly not on academia (I wrote about that in few places - I have not got any course - obligatory or elective - which even slightly touches the topic of functional programming during my studies), but when I was trying various combinations of ... *window managers* for *Linux*.

<img class="right haskell-logo" alt="Haskell Logo" src="/assets/HaskellLogo.png" />

After few huge fights with *KDE*, *GNOME* and *Xfce*, I become a very enthusiastic fan of *Fluxbox*. But I struggled with this topic more and more, and I have discovered *tiling window managers* family - with its representative called [Awesome](http://awesome.naquadah.org). I worked with it for couple of years, then I have started working as *C# programmer* which required switching from *Linux* to *Windows* and I had to deal with multiple inconveniences of the latter *OS*. When I returned to the *Linux*, it was a default choice for me that I have to use *tiling window manager*. But before I chosen blindly again the same one, I have done a research. And I found [XMonad](http://www.xmonad.org). It is very similar to my previous choice, but it is written in *Haskell* - and your *configuration files* are written also in that language.

After a few days of reading documentation, learning about the language and concepts which are embedded inside the *configuration file* "`DSL`", I have managed to configure all things which was necessary for me (like *tray*, *main bar*, *multiple workspaces*, *multi-head display* and so on). And this is how I started to gain interest in the language itself.

Second time, I have experienced *Haskell* via an aforementioned book - I partially agree with author's choice for that language (Bruce Tate chosen [*Spock*](https://en.wikipedia.org/wiki/Spock) as a *Haskell* representative), but definitely some features are common for both (like *purity*, *idealistic approach to everything* and *exactness*). After that, I wanted to learn the language in a more *structured manner*.

### Learn You a Haskell for Great Good!

And that leaded to my third *language rediscovery* with an amazing book titled [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters)

This book approaches the topic in a different way. It starts gently, without throwing at you too much of mathematical jargon, but in the end it introduces you to various mathematical constructs. *I really like the examples* and flow through the book. It is really sad that it ends so early of the topic's space. :wink:

### Learning Experience

We touched that a little bit already. Language is really *hard to start*, from almost everywhere - documentation, blog posts, books - you are under attack of various mathematical concepts and theories. It is even worse, if you are a novice *functional programmer* - because by that it will introduce another cognitive load for your brain. But it is *very rewarding after all*. It is like doing really hard puzzle or comprehensive workout - it is hard, but after dealing with it you will gain an *endorphin rush*, because *you have finally made it*!

### Type Inference

I would like to point out one more thing - the type system and inference is pushed on the higher level. You can only feel that by doing examples. Languages like *Java* or *C#* are really toys in a comparison to *Haskell*.

### Elegance and Conciseness

It is not a coincidence that language mirrors many mathematical concepts in an elegant and concise syntax. It may look cryptic at the beginning, but it will be easier with each step. Of course you have got available other standard concepts like *pattern matching*, *recursion* (with proper *tail call optimization*) etc.

{% highlight haskell linenos %}
count 0         _   = 1
count _ []          = 0
count x (c : coins) = sum [ count (x - (n * c)) coins | n <- [ 0 .. (quot x c) ] ]
 
main = print (count 100 [ 1, 5, 10, 25, 50 ])
{% endhighlight %}

Not only a *type inference* is an intelligent feature of the language. Another example is related with *ranges*, like in the *math* you can specify only couple first elements which are sufficient to deduce the rest of the sequence. Speaking of the *mathematical syntax* - in *Haskell* function composition is represented as `.` operator (*a dot*, very similar to the corresponding math symbol). Another example of elegant syntax is a *function application with enforced precedence*. It is represented as a dollar `$` (in that case it is only a convenience without math equivalent, but hidden in a nice operator syntax).

{% highlight haskell linenos %}
ghci> take 50 $ filter even [3,4..]
ghci> take 50 [ x | x <- [3,4..], even x]
{% endhighlight %}

### Other examples

I did not mention deliberately many other things (like *type classes*) which are making this language really unique. Otherwise, blog post will be much longer than a simple overview. Before we will finish, I would like to encourage you to do a small exercise.

First, I would like to that you will read chapter about [*functors, applicative functors and monoids*](http://learnyouahaskell.com/functors-applicative-functors-and-monoids)(if you have not read this book yet, I encourage you to read it as a whole) and then approach the problem of printing out *tree structure* in different order. Example in the book is traversing the tree only *in order*. Try to come up with other traversal types - *pre* and *post* order - which are based on the same mechanism with `Foldable`. It is really rewarding experience, you need to thing about certain things on a different level. In the example below you can see my solution.

*It is very likely that this is unidiomatic Haskell code - you have been warned*. :wink:

{% highlight haskell linenos %}
import Data.Monoid
import qualified Data.Foldable as F

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

newtype InOrderTree a = InOrderTree { getInOrderTree :: Tree a }
newtype PreOrderTree a = PreOrderTree { getPreOrderTree :: Tree a }
newtype PostOrderTree a = PostOrderTree { getPostOrderTree :: Tree a }

instance F.Foldable Tree where
    foldMap f Empty        = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r

instance F.Foldable InOrderTree where
    foldMap f (InOrderTree Empty)        = mempty
    foldMap f (InOrderTree (Node x l r)) = F.foldMap f (Node x l r)

instance F.Foldable PreOrderTree where
    foldMap f (PreOrderTree Empty)        = mempty
    foldMap f (PreOrderTree (Node x l r)) = f x           `mappend`
                                            F.foldMap f l `mappend`
                                            F.foldMap f r

instance F.Foldable PostOrderTree where
    foldMap f (PostOrderTree Empty)        = mempty
    foldMap f (PostOrderTree (Node x l r)) = F.foldMap f l `mappend`
                                             F.foldMap f r `mappend`
                                             f x

postOrder = PostOrderTree (
             Node 5
              (Node 3
                (Node 1 Empty Empty)
                (Node 6 Empty Empty)
              )
              (Node 9
                (Node 8 Empty Empty)
                (Node 10 Empty Empty)
              )
           )

preOrder = PreOrderTree (
             Node 5
              (Node 3
                (Node 1 Empty Empty)
                (Node 6 Empty Empty)
              )
              (Node 9
                (Node 8 Empty Empty)
                (Node 10 Empty Empty)
              )
           )

inOrder = InOrderTree (
            Node 5
             (Node 3
               (Node 1 Empty Empty)
               (Node 6 Empty Empty)
             )
             (Node 9
               (Node 8 Empty Empty)
               (Node 10 Empty Empty)
             )
          )

sum = F.foldl (+) 0 inOrder
product = F.foldl (*) 1 inOrder

anyEqualTo3 = getAny $ F.foldMap (\x -> Any $ x == 3) inOrder

inOrderList = F.foldMap (\x -> [ x ]) inOrder
preOrderList = F.foldMap (\x -> [ x ]) preOrder
postOrderList = F.foldMap (\x -> [ x ]) postOrder
{% endhighlight %}

### What is next?

We reached the end of the first book. But it does not mean that there are no more languages to talk about. We will do a small break, maybe we will describe one or two more representatives which are not present in the sequel, and after that we will restart the same series, with a first language described in the [Seven More Languages in Seven Weeks](https://pragprog.com/book/7lang/seven-more-languages-in-seven-weeks).

I can tell you right now, that it will be *Lua* (which is *BTW* silently omitted in that blog post - [Awesome](https://en.wikipedia.org/wiki/Awesome_(window_manager)) is partially written in that language and configuration is also in that language). I hope to see you at the beginning of the *old* / *new* series! :smile:

### Credits

- [Haskell](https://www.haskell.org/)
- [Spock](https://en.wikipedia.org/wiki/Spock)
- [Seven Languages in Seven Weeks](https://pragprog.com/book/btlang/seven-languages-in-seven-weeks), *Bruce A. Tate*
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters)
- [XMonad](http://www.xmonad.org)
