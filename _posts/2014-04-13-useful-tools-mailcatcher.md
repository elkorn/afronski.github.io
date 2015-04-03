---
layout: post
title: Useful tools - mailcatcher
date: 2014-04-13T14:00+0200
---

# Useful tools - `mailcatcher`

## Overview

*MailCatcher* runs a super simple `SMTP` server which catches any message sent to it to display in a web interface. It is a very useful tool which does not need a huge knowledge about e.g. `sendmail` and it is really easy to set up.

## Installation and Usage

Service is written in *Ruby* and it is accessible in the gems repository. If you have *Ruby* installed you will only need to perform two installation steps:

1. Install it from gems by `gem install mailcatcher`.
2. Start the service by invoking command `mailcatcher`.
3. Go to the dashboard at `http://localhost:1080/`.
4. Send email through `smtp://localhost:1025` - after that email should appear in the dashboard.

Using it inside a web framework e.g. *`Symfony2`* is a piece of cake. The only thing you need is a small modification in the *`parameters.yml`* configuration file:

{% highlight yaml %}
parameters:
  mailer_transport:  smtp
  mailer_host:       localhost
  mailer_port:       1025
{% endhighlight %}

You need to be sure that in the *`config.yml`* you have these keys:

{% highlight yaml %}
swiftmailer:
  transport:      %mailer_transport%
  host:           %mailer_host%
  port:           %mailer_port%
{% endhighlight %}

After applying mentioned changes and rebuilding the cache application will send emails via `SMTP` server from *mailcatcher*.

If you need to run it as a service with web interface accessible outside you need to run it in this way:

{% highlight bash %}
~ $ nohup mailcatcher --http-ip 0.0.0.0 > /var/log/mailcatcher.log 2>&1 &
{% endhighlight %}

# Links

1. [`mailcatcher`](http://mailcatcher.me/)
