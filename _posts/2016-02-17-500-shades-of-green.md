---
layout: post
title: 500 shades of Green
date: 2016-02-17T23:30+0100
tags:
  - github
  - habits
  - software-craftsmanship
  - habits
---

# 500 shades of Green

![Public contributions](/assets/PublicContributionsGithub.png)
![Private contributions](/assets/PrivateContributionsGithub.png)

No, it is not fake. I did it - I have managed to have real streak which last 500 days in a row and document it on Github. But, you can do it in a different way - there are couple of projects which allows you to create arbitrary history of your contributions (it uses a separate repository for that and well - it cheats using ability of `git` to bend the time).

One is [here](https://github.com/gelstudios/gitfiti). :wink:

No, it does not mean anything. I am not a better programmer than anyone else, definitely quantity does not involve quality (I will describe this in details below).

No, it does not mean that I did not have proper holidays or that I have spent all my free time on that.

## Why did you do that?

Mainly because of two reasons: *documentation* and *creating habits*.

Many of the repositories like [afronski/playground-notes](https://github.com/afronski/playground-notes), [afronski/dotfiles](https://github.com/afronski/dotfiles) or [afronski/papers_i_love](https://github.com/afronski/papers_i_love) are pure and private documentation of my notes, publicly visible settings and observations with commit history.

It also helped me when it comes to developing new habits:

- Taking care about commit history, proper commit messages, linear and nice history. Working with facilities like *rebase*, *squash* or *fix-ups* helped me a lot in daily job.
  - Besides that some commits have attached story with explanation why I applied that change. It is sometimes priceless, because I can drop my mental overhead related to *fear of forgetting something important*. And when I hit the same wall again I can tackle it with certainty and exact solution. Example [here](https://github.com/afronski/dotfiles/commit/12305ca407a1fed3b2aafb0f3ccb194d8f429c81).
- Also having a daily habit of delivering something valuable is crucial. It helps a lot when it comes to daily work, when it comes to planning the workload, actual steps to be done.
  - It also helps regarding planning my weeks and during a lot of other related activities - like meet-up organization, building community around them or preparing real contributions to *open source projects*.
- Another point is related with developing other habits. It is easier to develop a new habit on top of existing foundation.

I cannot overlook the *fun factor* and that *all the small things* delivered daily sometimes formed something bigger, and *endorphin's rush* related with a finished project cannot be underestimated.

## God Mode

Obviously, if you know `git` well you can cheat with the dates (e.g. when you skipped a day or two) - especially if that repository (or branch) is not used by anyone:

{% highlight bash %}
~ (my-private-branch) $ git commit --amend --date="Wed Feb 01 23:50 2016 +0100"
{% endhighlight %}

All rules regarding rewriting commit history of pushed shared branches are applicable also in this case, so you have been warned. :wink:

## Quality != Quantity

Last, but not least - my daily streak means literally *nothing* when it comes to my skills. It can be only a indicator of my *stubborn nature* and *persistence*. Also, there are a lot of days which are really low when it comes to contributions value (like changing a thing or two in a `README.md` file). That's definitely not a full-blown contribution, but it still helps preserving the habit and it keeps you in the loop.

Hey - it still counts if you feel better afterwards! *Why so serious*? :wink: It is just a pile of green squares. Just don't break the chain, and try as much as you can to deliver something - it will be still awesome, even if it will serve purpose only for you. For me it is like a *motivational perpetuum mobile* - I wish you the same thing!
