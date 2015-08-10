---
layout: post
title: Using JIRA from the command line
date: 2014-04-02T15:00+0200
tags:
  - jira
  - nodejs
  - shell
---

# Using `JIRA` from the command line

If someone would like to automate some popular workflows related with `JIRA` (like logging time related with code review or meetings) or just use it from command line, please look at [this tool](http://tebriel.github.io/jira-cli).

Installation:

{% highlight bash %}
~ $ npm install -g jira-cli
{% endhighlight %}

Then create a file inside your home directory called *`.jiraclirc.json`* with content:

{% highlight javascript %}
{
  "user": "USERNAME",
  "password":"PASSWORD",
  "host": "address.to.jira.com",
  "protocol": "https:",
  "port": 443,
  "strictSSL": false,
  "project": PROJECTID
}
{% endhighlight %}

After that you can log time with command specified below. In this case, it will prompt you to type a work log comment and amount of time spent with this activity e.g. *`1h`* (format like in corresponding dialog):

{% highlight bash %}
~ $ jira -w BLAHBLAH-111
{% endhighlight %}

# Links

1. [`jira-cli`](http://tebriel.github.io/jira-cli/)
