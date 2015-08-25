---
layout: post
title: Seven Languages in Seven Weeks - Haskell
date: 2015-08-26T16:00+0200
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

I was aware of *Haskell* existence a long time ago (around 2009 or 2010). At that time, I was completely sure that it is a *purely academical programming language* with no actual industrial usage. **I could not be more wrong**.

My first actual experience, when I rediscovered *Haskell* was surprisingly not on academia (I wrote about that in few places, but I have not got any course - obligatory or elective - which even slightly touches the topic of functional programming during my studies), but when I was trying various combinations of ... *window managers* in *Linux* space.

<img class="right haskell-logo" alt="Haskell Logo" src="/assets/HaskellLogo.png" />

After few huge fights with *KDE*, *GNOME* and *Xfce*, I become a very enthusiastic fan of *Fluxbox*. But I struggled with this topic more and more, and I have discovered *tiling window managers* family - with its representative called [Awesome](http://awesome.naquadah.org). I worked with it couple of years, then I have started working as *C# programmer* which requires switching from *Linux* to *Windows* and I had to deal with multiple inconveniences of that *OS*. When I returned to the *Linux*, it was a default choice for me that I have to use *tiling window manager*. But before I chosen blindly again the same one, I have done a research. And I found [XMonad](http://www.xmonad.org). It is very similar to my previous choice, but it is written in *Haskell* - and that applies also to your *configuration files*.

After a few days of reading documentation, learning about the language and concepts which are embedded inside the *configuration file* `DSL`, I have managed to configure all things which was necessary for me (like *tray*, *main bar*, *multiple workspaces*, *multihead display* and so on). And this is how I started to gain interest in the language itself.

My second experience is an aforementioned book - and I partially agree and disagree which author's choice for that language (Bruce Tate chosen [*Spock*](https://en.wikipedia.org/wiki/Spock) as a *Haskell* representative), but definitely some character features are common for both (like the *purity*, *idealistic approach to everything* and *exactness*). After that, I wanted to learn the language in a more *structured manner*.

### Learn You a Haskell for Great Good!

And that leads to my third *language rediscovery* - with an amazing book titled [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters)

*TODO*: Describe how book approach the topic, how it introduces you to various mathematical constructs (very gently). **I really like the examples**.

### Learning Experience

Language is really *hard to start*, from almost everywhere - documentation, blog posts, books - you are under attack of various mathematical concepts and theories. It is even worse, if you are a novice in *functional programming* - because it will introduce another cognitive load on your brian. But it is *very rewarding after all*. It is like doing really hard puzzle or comprehensive workout - it is hard, but after dealing with it you will gain an *endorphin rush*, because *you have made it*!

### Type Inference

*TODO*: Amazing thing, type inference in languages like *Java* or *C#* is really a toy in comparison to *Haskell*.

### Elegance and Conciseness

*TODO*: Explain it with example related to odd / even filtering, or even `sum-coins.hs` from SICP.

{% highlight haskell linenos %}
ghci> take 50 $ filter even [3,4..]
ghci> take 50 [ x | x <- [3,4..], even x]
{% endhighlight %}

{% highlight haskell linenos %}
count 0         _   = 1
count _ []          = 0
count x (c : coins) = sum [ count (x - (n * c)) coins | n <- [ 0 .. (quot x c) ] ]
 
main = print (count 100 [ 1, 5, 10, 25, 50 ])
{% endhighlight %}

### Typeclasses FTW!

*TODO*: Explain what is a *typeclass*, why it is different and how it will help programmers on the higher level.

### Other examples

This section is very likely to has an *unidiomatic Haskell code* - you have been warned. :wink:

*TODO*: Explain where the original example is, and why it is fun to play with. And what the `Foldable` is.

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

*TODO*: Describe source code in details.

### What is next?

We reached the end of the first book. But it does not mean that there are no more languages to talk about. We will do a small break, maybe we will describe one or two more representatives which are not present in the sequel, and after that we will restart the same series, with a first language described in the sequel called [Seven More Languages in Seven Weeks](https://pragprog.com/book/7lang/seven-more-languages-in-seven-weeks).

It will be *Lua* (which is *BTW* silently omitted in that blog post - [Awesome](https://en.wikipedia.org/wiki/Awesome_(window_manager)) is partially written in that language and you are writing a configuration in it as well). I hope to see you at the beginning of the *old* / *new* series! :smile:

### Credits

- [Haskell](https://www.haskell.org/)
- [Spock](https://en.wikipedia.org/wiki/Spock)
- [Seven Languages in Seven Weeks](https://pragprog.com/book/btlang/seven-languages-in-seven-weeks), *Bruce A. Tate*
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/chapters)
- [XMonad](http://www.xmonad.org)
