---
layout: post
title: Low-level Node.js debugging
date: 2013-11-02T10:30:00Z
---

# Low-level Node.js debugging

Sometimes when you want to run your Node.js application, you will receive this:

{% highlight bash %}
node-stuff $ node index.js
Segmentation fault
{% endhighlight %}

Yeah... and *now* what?

First of all, if you were deployed an application on *OS* different than *SmartOS*, you loose ability to use many impressive tools which will definitely help you in that case (most impressive ones are `mdb` and `dtrace`, you can find details in references section).

So no *`mdb`*, no *`dtrace`*, no *SmartOS*. We have got only `gdb` and our brains filled with low-level computer knowledge :wink:.

Before we delve deep into internals of our application, let's begin the voodoo, that is a process of setting proper options in our *OS*. First we will remove the limit related with *core dump* size:

{% highlight bash %}
~ $ sudo ulimit -c unlimited
{% endhighlight %}

Next we set different path for these files (remember to put them on partition with a huge amount of the free disk space):

{% highlight bash %}
~ $ echo '/tmp/core_%e.%p' | sudo tee /proc/sys/kernel/core_pattern
{% endhighlight %}

Then, if we are using native Node.js extensions we have to recompile them with the debugging flag *`-g`* (configuration file taken from *`node_xslt`* module):

{% highlight javascript %}
{
  "targets": [{
    "target_name": "node_xslt",
    "sources": [ "node_xslt.cc" ],

    "cflags": [
      "&lt;!(xml2-config --cflags)",
      "-fexceptions",

      "-g"
    ],

    "cflags_cc": [
      "&lt;!(xml2-config --cflags)",
      "-fexceptions",

      "-g"
    ],

    "xcode_settings": {
      "OTHER_CFLAGS": [
        "&lt;!(xml2-config --cflags)",
        "-fexceptions",

        "-g"
      ]
    },
    "libraries": ["-lxml2" , "-lxslt" , "-lexslt"],
    "library_dirs": [ "/usr/lib" ]
  }]
}
{% endhighlight %}

Then, look at the dependencies (in our case `libxslt`, `libxml2`) and reinstall them in debug version (or `-devel` as well).

After modifications and installing dependencies with debugging mode, we have to run installation procedure inside directory of the modified module:

{% highlight bash %}
node_xslt $ npm install --verbose
{% endhighlight %}

Somewhere inside verbose log messages we will find information about flags used in compilation process.

Next, we have to simulate again *`Segmentation fault`* behavior. When application receive proper signal, it will dump file with the *post-mortem* internal structure.

{% highlight bash %}
~ $ gdb /usr/bin/node /tmp/core_XXX.YYY
{% endhighlight %}

And now we are inside the belly of the monster, so we can move around:

{% highlight bash %}
...
Program received signal SIGSEGV, Segmentation fault.

(gdb) bt
# Shows the stack trace (but from the native code).
(gdb) print V8_Fatal("a", 11, "c")
# Shows the stack trace inside V8.
(gdb) quit

~ $
{% endhighlight %}

If you want to attach to running process, you have to invoke command specified below:

{% highlight bash %}
~ $ gdb attach $(pidof node)
{% endhighlight %}

However, if your application is running in cluster mode, this will fail and instead of *`pidof`* you have to pass a single *PID* value (for master or one of the slaves, depends on what is interesting for you).

You can also run `strace` in order to determine which system calls your program invoke before *death*:

{% highlight bash %}
~ $ strace -ttT
{% endhighlight %}

After gathering certain amount of knowledge, armed with stack traces, system calls invocations, memory footprint you have to *dive deeper* into actual application code.

Welcome to the dungeon, marine :grin:.

# References

1. [Using `mdb`](http://dtrace.org/blogs/dap/2012/01/13/playing-with-nodev8-postmortem-debugging)
2. [Using `dtrace`](http://dtrace.org/blogs/dap/2012/01/05/where-does-your-node-program-spend-its-time)
3. [`ulimit` man pages](http://linux.die.net/man/3/ulimit)
4. [node_xslt](https://github.com/bsuh/node_xslt)
5. [`strace` man pages](http://linux.die.net/man/1/strace)