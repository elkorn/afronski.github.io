---
layout: post
title: Easter Eggs in Python's REPL
date: 2014-06-08T10:10+0200
---

# Easter Eggs in Python's `REPL`

Recently, I have ran into a funny thing in Python. Try to import *`antigravity`* module inside the `REPL`:

{% highlight python %}
>>> import antigravity
{% endhighlight %}

Obviously, it is not the only funny thing hidden somewhere deep in the Python implementation. I researched the topic and I found more examples of easter eggs :grin:.

The best known is `import this` easter egg:

{% highlight python %}
>>> import this
The Zen of Python, by Tim Peters

Beautiful is better than ugly.
Explicit is better than implicit.
Simple is better than complex.
Complex is better than complicated.
Flat is better than nested.
Sparse is better than dense.
Readability counts.
Special cases aren't special enough to break the rules.
Although practicality beats purity.
Errors should never pass silently.
Unless explicitly silenced.
In the face of ambiguity, refuse the temptation to guess.
There should be one-- and preferably only one --obvious way to do it.
Although that way may not be obvious at first unless you're Dutch.
Now is better than never.
Although never is often better than *right* now.
If the implementation is hard to explain, it's a bad idea.
If the implementation is easy to explain, it may be a good idea.
Namespaces are one honking great idea -- let's do more of those!
{% endhighlight %}

There is also nice [example](http://www.redmountainsw.com/wordpress/archives/a-joke-in-the-python-interpreter) which exploits idea of `this`:

{% highlight python %}
>>> love = this
>>> this is love
True
>>> love is True
False
>>> love is False
False
>>> love is not True or False
True
>>> love is not True or False; love is love
True
True
{% endhighlight %}

Also, Python as a *truly enterprise ready* language has built-in `Hello World` functionality:

{% highlight python %}
>>> import __hello__
Hello world...
>>> reload(__hello__)
Hello world...
<module '__hello__' from '<frozen>'>
{% endhighlight %}

But the funniest thing is *`IMHO`* this:

{% highlight python %}
>>> from __future__ import braces
  File "<stdin>", line 1
SyntaxError: not a chance
>>>
{% endhighlight %}

This exception message definitely made my day :wink:.

If I omitted something, please post a comment and let me know which I have missed.

# References

1. [`xkcd`: Python!](http://xkcd.com/353)
2. [Module `antigravity`](http://svn.python.org/view/python/trunk/Lib/antigravity.py?view=markup)
