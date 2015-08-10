---
layout: post
title: Mysterious problem with missing name of the active virtualenv
date: 2013-11-13T18:20+0100
tags:
  - shell
  - python
  - virtualenv
---

# Mysterious problem with missing name of the active `virtualenv`

Recently I have ran into an issue related to `virtualenv` and my shell. It did not display name of the active `virtualenv` after invoking the standard command:

{% highlight bash %}
source bin/activate
{% endhighlight %}

However, everything works perfectly without that (even a `deactivate` command). This little thing makes me nervous so I have started looking for a solution to that problem.

After a while I found the reason why it does not work:

{% highlight bash linenos %}
function calculate_prompt() {
  # Beautifying the prompt ;).
}

PROMPT_COMMAND=calculate_prompt
{% endhighlight %}

*Yeah*, I totally forgot about the fact that I am using `PROMPT_COMMAND`. Right now, when I know the reason, the solution is a piece of cake - just add the code for displaying *`virtualenv`* name to the function which calculates the prompt:

{% highlight bash linenos %}
function calculate_prompt() {
  # Beautifying the prompt ;).
  # ...

  if [ -z "$VIRTUAL_ENV_DISABLE_PROMPT" ] ; then
    _OLD_VIRTUAL_PS1="$PS1"

    if [ "x" != x ] ; then
        PS1="$PS1"
    else
      if [ ! -z "$VIRTUAL_ENV" ] ; then
        if [ "`basename \"$VIRTUAL_ENV\"`" = "__" ] ; then
          PS1="`basename \`dirname \"$VIRTUAL_ENV\"\``: $PS1"
        else
          PS1="`basename $VIRTUAL_ENV`: $PS1"
        fi
      fi
    fi
  fi
}

PROMPT_COMMAND=calculate_prompt
{% endhighlight %}

And it is fixed! :wink:
