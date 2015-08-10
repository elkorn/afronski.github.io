---
layout: post
title: My picks of NDC 2014 - part I
date: 2014-06-16T22:10+0200
tags:
  - conferences
  - videos
  - NDC
---

# My picks of NDC 2014 - part I

1. [Kevlin Henney - Seven Ineffective Coding Habits of Many Programmers](https://vimeo.com/97329157) - Very refreshing talk about style and programming habbits. Even if it has many controversial points from your perspective and it is very opinionated, many of them are at least thought-provoking and maybe they should be reconsidered and rethought again.
2. [Hadi Hariri - Developing in a Decade](https://vimeo.com/97315946) - Very inspiring and refreshing talk what software development, internet and whole IT actually is and why it should change if we want to be happier and live in the better world. Amazing anegdotes and very thought-provoking comparisons to the books (Orwell's *"1984"*, Huxley's *"Brave New World"*, Postman's *"Amusing Ourselves to Death"*). For me, this is the best presentation of this year NDC by now.
3. [Robert C. Martin - Clean Architecture and Design](https://vimeo.com/97530863) - Strongly recommended if you have not seen earlier talks made by *Uncle Bob* related with this topic. The most appropriate summary of this talk: *application's business rules are the most important things, protect them - everything is a implementation detail and it should be pluggable*. In other words *good architecture maximizes the number of decisions not made*.
4. [Tim Berglund - Discrete Math You Need to Know](https://vimeo.com/97505656) - Inspiring talk (even if you may think that math cannot be inspiring). Definitely recommended, at first - it is recommended to stretch your mind frequently, moreover - it is nice to know more and have these tools in your toolbox (or what is even more recommended - refresh it). Surprisingly, still many people in IT do not know how the RSA works underneath.
5. [Denise R. Jackobs - Banish Your Inner Critic](https://vimeo.com/97318800) - It can be very enlightening talk for many people, maybe even a life changer. If you have problems with pursuing to the perfection state, procrastination and you have feeling and knowledge that you are a victim of *impostor syndrome* definitely worth watching.
6. [Robert C. Martin - Advanced Test Driven Development](https://vimeo.com/97516288) - Again, if you have seen it before, there is probably nothing new, but if not - please definitely watch it. Very interesting concept called *transformation priority premise*. And surprising summary - *if statement is a degenerated case of while loop*. :wink:
7. [Scott Bellware - TDD in Tatters](https://vimeo.com/97537026) - Eye-opening introduction about cognitive theory and learning. Very nice differentiation between crude and subtle things, which sometimes fells out and disappear in the process of learning. I disagree about that automation leads to removal a connection between the programmers and code, but still it is a viable talk (at least the beginning about *amateur cognitive theory*, pointing out the subtleties and explaining the design role in *TDD* acronym). The most important quote from this presentation is *every test is an additional coupling to the code* - and by providing good design we should minimize the coupling, we should control it. It is the subtle thing that we often ignore.

## How to watch these videos offline?

Unfortunately, since 2013 there are no *`torrent`* files, which combine all videos in one place. But there is a solution - we can use `Selenium` *JavaScript* driver to scrap all links from *`HTML5`* *video* tags. Complete source code of scripts and helpers is available [here](https://github.com/afronski/playground-repository/tree/master/vimeo-scraper). Installation:

{% highlight bash linenos %}
# On Arch Linux, Selenium Chrome WebDriver is available only in AUR.
~ $ pacaur -S chromedriver
~ $ npm install
~ $ node vimeo-scraper.js > urls.txt
{% endhighlight %}

In the output file you will receive list of URLs which point directly to *MP4* ready to download.

## Appendix - A bitter state of headless browsers

I know that there are tools capable to do such things headlessly. I thought that it will be an easy task to do.

Unfortunately, I have tested `Phantom.js` and `SlimmerJS` (representatives of two major browser engines - *WebKit* and *Gecko*) and both does not work (even if creators of `SlimmerJS` [claims](http://slimerjs.org/features.html) that it should work with *`HTML5`* *video*, `Phantom.js` is slightly better in this matter - it [officially](https://github.com/ariya/phantomjs/wiki/Supported-Web-Standards#unsupported-features) does not support *audio* and *video*).

The best implementation of *`WebKit`* rendering engine available to Node.js called *Chimera* is [no longer maintained](https://github.com/deanmao/node-chimera/issues/44#issuecomment-30561620). It was a very nice module, I have used it in the past - very unfortunate and disappointing decision.

So if you really want to scrape pages headlessly by doing more than simple rendering, measure loading time and capture a single screenshot, you need to use the good, old and reliable pair: `Selenium` and `Xvfb`.

# References

1. [NDC 2014 Vimeo Playlist](https://vimeo.com/channels/ndc2014)
2. [SlimmerJS](http://slimmerjs.org)
3. [phantomjs](http://phantomjs.org)
4. [`node-chimera`](https://github.com/deanmao/node-chimera)
