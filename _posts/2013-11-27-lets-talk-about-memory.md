---
layout: post
title: Let's talk about memory
date: 2013-11-27T23:35+0100
tags:
  - browsers
  - firefox
  - debugging
---

# Let's talk `about:memory`

Recently on polish Facebook group *JSNews*, thanks to [Zbigniew Braniecki](https://twitter.com/zbraniecki), I found a very useful tool for monitoring memory in Firefox - `about:memory`.

![Overview for about:memory](/assets/AboutMemoryOverview.png)

You can open it by typing `about:memory` in your address bar. It has capabilities to measure memory usage at browser level, load and save dumps (as a gzipped *`JSON`* file), compare them and force garbage and cycle collection.

![Difference for two memory dumps](/assets/AboutMemoryDiff.png)

I strongly encourage you to dive into that tool, especially if you like me, thought that *Firefox* hasn't got any tools for memory profiling. Well, not the first time I'm wrong :wink:.

P.S. On screenshots you can observe new UI for *Firefox*, called *Australis* - what do you think about it? Share your opinion in comments.
