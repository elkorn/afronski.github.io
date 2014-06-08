---
layout: post
title: My picks of NDC 2014
date: 2014-06-11T16:00:00Z-02:00
---

# My picks of NDC 2014

1. [Kevlin Henney - Seven Ineffective Coding Habits of Many Programmers](https://vimeo.com/97329157) - Very refreshing talk about style and programming habbits. Even if it has many controversial points from your perspective and it is very opinionated, many of them are at least thought-provoking and maybe they should be reconsidered and rethought again.
2. [Hadi Hariri - Developing in a Decade](https://vimeo.com/97315946) - Very inspiring and refreshing talk what software development, internet and whole IT actually is and why it should change if we want to be happier and live in the better world. Amazing anegdotes and very thought-provoking comparisons to the books (Orwell's *"1984"*, Huxley's *"Brave New World"*, Postman's *"Amusing Ourselves to Death"*). My very first association with this talk was *Roger Water's* lyrics from the song *"Amused to death"*.
3. [Robert C. Martin - Clean Architecture and Design](https://vimeo.com/97530863) - Strongly recommended if you have not seen earlier talks made by *Uncle Bob* related with this topic. The most appropriate summary of this talk: *application's business rules are the most important things, protect them - everything is a implementation detail and it should be pluggable*. In other words *good architecture maximizes the number of decisions not made*.

# How to watch these videos offline?

Unfortunately, since 2011 there are no *`torrent`* files, which combine all videos in one place. But there is a solution - we can use `Selenium` *JavaScript* driver to scrap all links from *`HTML5`* *video* tags.

{% highlight javascript linenos %}
{% endhighlight %}

{% highlight bash linenos %}
# Selenium Chrome WebDriver is available only in AUR.
~ $ pacaur -S chromedriver
~ $ mkdir vimeo-scraper && pushd vimeo-scraper
~ $ touch vimeo-scraper.js
# Paste script from above to the created JavaScript file.
~ $ npm install selenium-webdriver
~ $ node vimeo-scraper.js > urls.txt
{% endhighlight %}

In the output file you will receive list of URLs which point directly to *MP4* ready to download.

# Appendix - A bitter state of headless browsers

I know that there are tools capable to do such things headlessly. I thought that it will be an easy task to do.

Unfortunately, I have tested `Phantom.js` and `SlimmerJS` (representatives of two major browser engines - *WebKit* and *Gecko*) and both does not work (even if creators of `SlimmerJS` [claims](http://slimerjs.org/features.html) that it should work with *`HTML5`* *video*, `Phantom.js` is slightly better in this matter - it [officially](https://github.com/ariya/phantomjs/wiki/Supported-Web-Standards#unsupported-features) does not support *audio* and *video*).

The best implementation of *`WebKit`* rendering engine available to Node.js called *Chimera* is [no longer maintained](https://github.com/deanmao/node-chimera/issues/44#issuecomment-30561620). It was a very nice module, I have used it in the past - very unfortunate and disappointing decision.

So if you really want to scrape pages headlessly by doing more than simple rendering, measure loading time and capture a single screenshot, you need to use the good, old and reliable pair: `Selenium` and `Xvfb`.

# References

1. [NDC 2014 Vimeo Playlist](https://vimeo.com/channels/ndc2014)
2. [SlimmerJS](http://slimmerjs.org)
3. [phantomjs](http://phantomjs.org)
4. [`node-chimera`](https://github.com/deanmao/node-chimera)