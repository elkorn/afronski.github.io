---
layout: post
title: Tips and tricks for chef-solo and vagrant
date: 2013-10-18T20:10:00Z
---

# Tips and tricks for chef-solo and vagrant

In your daily work with the kitchen sink :wink:, you have to be a *plumber* and get dirty. In order to do that you have to be familiar with the concept of debugging *vagrant*, *chef-solo* and some useful tricks.

## Debugging vagrant

If we want to enable a verbose mode for *vagrant*, we need to set an environment variable `VAGRANT_LOG` with `DEBUG` or `INFO` value, as in the example below:

{% highlight bash %}
~ $ VAGRANT_LOG=DEBUG vagrant up
{% endhighlight %}

## Debugging chef-solo

We have got two choices to enable verbose mode for *chef-solo*.

First, we can login into a created machine via *SSH* and invoke the *magic command* in a certain directory:

{% highlight bash %}
~ $ vagrant ssh

vagrant@localhost ~ $ cd /tmp/vagrant-chef-1
vagrant@localhost /tmp/vagrant-chef-1 $ sudo chef-solo -c solo.rb -j dna.json -l debug
{% endhighlight %}

Parameter `-c` is for passing a configuration file, `-j` is for passing a file with attributes, further called a node specification. Last one, most important for us, `-l` enables the verbose mode at specified logging level.

You may be wondering, why we are entering to the directory `/tmp/vagrant-chef-1` with a digit at the end. Actual value depends on number of concurrent and active provisioning processes running on the specified machine and it is represented by [get_and_update_counter](https://github.com/mitchellh/vagrant/blob/master/plugins/provisioners/chef/provisioner/base.rb#L22) method and [Counter](https://github.com/mitchellh/vagrant/blob/master/lib/vagrant/util/counter.rb) module.

But we can do it another way, by passing the arguments to a provisioner inside *Vagrantfile*:

{% highlight ruby linenos %}
Vagrant.configure('2') do |config|
  # ...

  config.vm.provision :chef_solo do |chef|
    # ...

    chef.arguments = '-l debug'
  end
end
{% endhighlight %}

## Mutating the DNA

As you can see, in the example above related with chef-solo debugging, we have got a `dna.json` file with attributes. We call it a node specification. But what if we already have an existing node specification and we want to use it?

There is an attribute for that, called `json`, but it is a hash (it not a string like in the *add_role* method or setting the *environment* name). In order to inject our attributes saved in file we need to read it, parse it as a JSON and extend the existing field:

{% highlight ruby linenos %}
Vagrant.configure('2') do |config|
  # ...

  config.vm.provision :chef_solo do |chef|
    # ...

    chef.json.merge!(JSON.parse(File.read('path/to/json/file')))
  end
end
{% endhighlight %}

# Links

1. [Vagrant Github repository](https://github.com/mitchellh/vagrant)
2. [Vagrant documentation](http://docs.vagrantup.com/v2/)