---
layout: post
title: nodeschool.io
date: 2014-07-18T15:00:00Z-02:00
---

# nodeschool.io

I would like to present very interesting initiative for everyone interested in topics oriented around Node.js. If you are curious how to learn effectively about stuff available on the platform, how to use libraries effectively and write the most idiomatic code which is consistent with *UNIX* philosophy, please read this article further.

# Why?

The major problem when you are learning new stuff is related with first step, even with a basic question *"how to start"*. This barrier is often related with new thinking or programming paradigm, new language, unknown environment or even unfamiliar `API`. For almost every technology we can find opinionated stuff on the Internet: multiple tutorials, guidelines (often it is not so hard to find two conflicting ones :wink:) and blog posts with information how to work with technology in which we are interested. Moreover, in almost each case we still have a question back in our heads: *"Is is a proper way to learn and work with it?"*.

Learning process could be easier if we have one source of truth and best practices how to approach certain topics related with piece of technology which is interesting for us. For Node.js such initiative is called [nodeschool.io](http://nodeschool.io).

![Nodeschool Main Page](/assets/nodeschool.png)

# What?

Nodeschool is a series of interactive workshops, divided into two sections - core concepts and electives.

In the main concepts we have:
- [basic asynchronous programming and introduction to the core Node.js concepts](http://nodeschool.io/#learn-you-node)
- [introduction to the streams](http://nodeschool.io/#stream-adventure)
- [bytes and buffers manipulation](http://nodeschool.io/#bytewiser)
- [functional programming in JavaScript](http://nodeschool.io/#functionaljs)
- [how to work effectively with git and github](http://nodeschool.io/#git-it)

Electives are oriented around:
- [use of node.js binding and modules related with LevelDB](http://nodeschool.io/#levelmeup)
- [hapi.js framework](http://nodeschool.io/#makemehapi)
- [express.js framework](http://nodeschool.io/#expressworks)
- [native modules and addons](http://nodeschool.io/#goingnative)
- [6th version of ECMAScript](http://nodeschool.io/#count-to-6)
- [browserify](http://nodeschool.io/#browserify-adventure)
- [promises](http://nodeschool.io/#promiseitwonthurt)
- [async.js](http://nodeschool.io/#asyncyou)

Every single workshop is using the same command line interface and workflow. This eases the work with each of them. Moreover assignments are well defined, descriptive and small enough to perform them as a *daily kata*.

Workflow is oriented around invoking commands in the terminal. After starting the workshop executable without any parameters you will see the list of available assignments, like presented below:

![learnyounode workshop](/assets/learnyounode.png)

Then you should select first unresolved assignment from the list. After that you will see the description and guidelines how you should approach this task. Then you have two commands for testing and verifying prepared solution.

{% highlight bash %}
# Testing your solution:
~ $ learnyounode run <FILE_NAME_WITH_SOLUTION>
# Veryfing the prepared solution:
~ $ learnyounode verify <FILE_NAME_WITH_SOLUTION>
{% endhighlight %}

If solution is correct, the current assignment will be marked as resolved. Now you are ready to work with each workshop available on the main page.

# I am the beginner in Node.js community - where should I start?

I will recommend to start with [learn-you-node workshop](http://nodeschool.io/#learn-you-node) and then move forward through the rest of core concepts. After that you are free to choose any from the interesting topics from electives group (e.g. related with specific framework).

# I am more advanced member - how can I contribute?

You can contribute in several ways.

The easiest thing is to perform any workshop and check it in practice (especially with edge cases). Then you can fork repository related with it, fix the bugs, improve documentation and clarity of the steps, maybe even add missing scenarios and lessons.

Second option is related with local events. You can easily organize a new event in your city which will be oriented around whole *nodeschool* initiative (or even around just one type of workshops). There is a detailed guideline prepared by organization [how to do this](http://nodeschool.io/host.html).

And finally, you can create a new workshop. In such vibrant and broad community you can easily find topic suitable for you. If you do not have a clue which could be suitable for you, you can look at the list of possible topics gathered [here](https://github.com/nodeschool/workshoppers/issues?labels=status%3Arequested). If you already have an idea, you should fork the [rvagg/workshopper](https://github.com/rvagg/workshopper) project and start creating lessons. Then, you can add your project to the list of available workshops via [github](https://github.com/nodeschool/workshoppers/issues?labels=status%3Ain+progress) - then your workshop will be reviewed, validated and after successful acceptance it will be available as a official lesson on the main *nodeschool* page.

# References

1. [nodeschool.io](http://nodeschool.io)
2. [How to run a new 'nodeschool' event?](http://nodeschool.io/host.html)
3. [rvagg/workshopper](https://github.com/rvagg/workshopper)