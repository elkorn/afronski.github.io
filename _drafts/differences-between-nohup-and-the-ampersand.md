---
layout: post
title: Differences between nohup and ampersand
---

# Differences between `nohup` and the ampersand

There are many cases when differences between environments can bite you. This is one on which I have ran into recently. What is the difference between these two commands?

{% highlight bash linenos %}
nohup myprocess.out &
myprocess.out &
{% endhighlight %}

The answer is the same as usual - *it depends*.

`nohup` catches the hangup signal while the ampersand does not.

What is the hangup signal?

*`SIGHUP` - hangup detected on controlling terminal or death of controlling process (value: 1).*

Normally, when running a command using `&` and exiting the shell afterwards, the shell will terminate the sub-command with the hangup signal (like `kill -SIGHUP $PID`). This can be prevented using nohup, as it catches the signal and ignores it so that it never reaches the actual application.

Fine, but like in this case there are always a 'buts'. There is no difference between these launching method when the shell is configured that way that it doesn not send `SIGHUP` at all.

In case you are using bash, you can use the command specified below to find out whether your shell sends *`SIGHUP`* to its child processes or not:

{% highlight bash %}
shopt | grep hupon
{% endhighlight %}

And moreover - there are cases where nohup does not work. For example, when the process you start reconnects the `NOHUP` signal (it is done inside, on the application code level).

In described case, lack of differences bite me when inside custom service launching script there was a call to the second script which sets up and launches the proper application without `nohup` command.

On the one Linux environment everything works smoothly, on the second one application quits as soon as the second script exits (detecting that case, of course took me much more time then you might think :stuck_out_tongue:).

# References

1. [`man 7 signal`](http://unixhelp.ed.ac.uk/CGI/man-cgi?signal+7)