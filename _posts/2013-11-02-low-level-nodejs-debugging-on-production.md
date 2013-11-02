---
layout: post
title: Low-level Node.js debugging on production
date: 2013-11-02T10:45:00Z
---

# Low-level Node.js debugging on production

Sometimes when you want to run your Node.js application on production environment, you will receive:

{% highlight bash %}
node-stuff $ node index.js
Segmentation fault
{% endhighlight %}

Yeah... and *now* what?

Welcome to the dungeon, *DevOp* marine :grin:.

First of all, if you deployed an application on *OS* different than *SmartOS*, you lost ability to use many impressive tools which will definitely help you in that case (most impressive ones are `mdb` and `dtrace`, you can find details in the references section of this post).

So there is no *`mdb`*, no *`dtrace`*, no *SmartOS*. You can't also use your favorite debugger like [trace.gl](https://trace.gl/), [node-monkey](https://github.com/jwarkentin/node-monkey) or [node-inspector](https://github.com/node-inspector/node-inspector). You have got only `gdb` and your brain filled with low-level computer knowledge :wink:.

Before we will dive deeper into internals of our application, let's begin the voodoo, that is a process of setting proper options in our *OS*. First we will remove the limit related with the *core dump* size:

{% highlight bash %}
~ $ sudo ulimit -c unlimited
{% endhighlight %}

Next, we set different path for these files (remember to put them on partition with a huge amount of the free disk space):

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

Then, look at the dependencies (in our case `libxslt`, `libxml2`) and reinstall them in the development version (sometimes marked with postfix `-devel`).

After modifications and installing dependencies in the *debugging mode*, we have to run installation process inside directory of the modified module (but only there, not on the upper level, because it will remove our changes in the modified module):

{% highlight bash %}
node_xslt $ npm install --verbose
{% endhighlight %}

Somewhere inside long output we will find information about flags used in compilation process (and we should see flag *`-g`*, responsible for attaching debugging informations).

Next, we have to simulate again situation with *`Segmentation fault`* behavior. When application receive unwanted signal, it will dump file with the *post-mortem* internal structure, ready for using inside `gdb`:

{% highlight bash %}
~ $ gdb /usr/bin/node /tmp/core_XXX.YYY
{% endhighlight %}

And now we are inside the belly of the monster, so we can move around:

{% highlight bash %}
...
Program received signal SIGSEGV, Segmentation fault.

(gdb) bt
# It shows the stack trace (but from the native code).
(gdb) print V8_Fatal("a", 11, "c")
# It shows the stack trace inside V8.
# Values "a", 11 and "c" are irrelevant.
(gdb) quit
# It exits gdb.
{% endhighlight %}

If you want to attach into the running process, you have to invoke the command specified below:

{% highlight bash %}
~ $ gdb attach $(pidof node)
{% endhighlight %}

However, if your application is running in a cluster mode (or you are running more than one Node.js application on your machine), this will fail and instead of *`pidof`* you have to pass a single *PID* value (from one application, master or one of the slaves, depends on what is interesting for you).

You can also run *`strace`* in order to determine which system calls your program invoke before *death*:

{% highlight bash %}
~ $ strace -ttT
{% endhighlight %}

After gathering certain amount of knowledge, armed with the stack traces, system calls invocations and memory footprint you have to *dig deeper* into the actual application code and maybe trying to reproduce that behavior in more *debuggable* environment :wink:.

# References

1. [Using `mdb`](http://dtrace.org/blogs/dap/2012/01/13/playing-with-nodev8-postmortem-debugging)
2. [Using `dtrace`](http://dtrace.org/blogs/dap/2012/01/05/where-does-your-node-program-spend-its-time)
3. [`ulimit` man pages](http://linux.die.net/man/3/ulimit)
4. [node_xslt](https://github.com/bsuh/node_xslt)
5. [`strace` man pages](http://linux.die.net/man/1/strace)