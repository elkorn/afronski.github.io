---
layout: post
title: Writing custom modules for Ansible
date: 2016-03-28T21:00+0200
tags:
  - ansible
  - erlang
  - devops
  - scripts
---

# Writing custom modules for Ansible

## Introduction

### What is Ansible and what is a `module`?

If you are a big fan of *automation*, if you are focused on spreading and building software in line with *DevOps* culture, tools related with automation / configuration management like *Chef* or *Ansible* are probably well known to you. From the other hand that what differentiates those tools is a topic for a next blog post. Without diving into those differences, let's briefly zoom into details for those people which do not know what *Ansible* and *modules* are.

*Ansible* is a free-software platform for configuring and managing machines. It combines deployment, ad-hoc tasks execution and configuration management. This tool uses *YAML* and declarative way of defining steps, which are modifying state of your fleet. *Module* is a single piece of those steps, a well defined way of executing certain tasks on the remote infrastructure. It executes commands, and communicates by outputting JSON to standard output - it means that it can be written in any programming or scripting language.

## Development

Before we'll start actual implementation we need to know how it works underneath.

### Requirements

If you want to write a custom module, we stated above that you have to be aware of two things. Your module code will be executed on the provisioned machine, **so all dependencies which your module requires, have to be there**. Second, your module communicates over specific input and output protocols. It uses certain syntax for sending input parameters (either sent as a `stdin` or a file) and JSON protocol which will be consumed as an output of the module. And nothing more - any other, non-JSON compliant output will be treated as an error, and would not be consumed by *Ansible* properly.

### Case Study

We would like to consume *XMLified* status pages and do certain actions based on checked and exposed facts, scraped from aforementioned place. Because I like Erlang, I will use that language to implement that module. We will do it using `escript`. What is it? Quoting official documentation:

<quote class="citation"><code>escript</code> provides support for running short Erlang programs without having to compile them first and an easy way to retrieve the command line arguments.</quote>

Besides way of executing code, we need to use an XML parser and HTTP client - in both cases we will use built-in thins from Erlang library - `xmerl` and `httpc`.

### Developing outside Ansible

First we need to setup our environment for Ansible. The easiest way to do it in Python world is to spin up a new `virtualenv`:

{% highlight bash %}
playground $ virtualenv -p python2.7 .local
playground $ source .local/bin/activate
playground $ pip install ansible==1.9.4                                            (.local)
{% endhighlight %}

Then we can fiddle with it.

#### Input arguments

How to pass an arguments to it? You can observe it either when you define task parameters inside *YAML* file or when you invoke ad hoc task in *Ansible*:

{% highlight bash %}
playground $ ansible host[001-002] -i production_hosts -m stat -a "path=/etc/passwd"
{% endhighlight %}

Everything stated inside `-a` attribute defines list of named parameters. In that case an argument named `path` has value `/etc/passwd`. This will be formatted and printed to a file. Path and name will be delivered as a first argument of command line invocation of our module.

#### Output format

As we specified above, our module will be executed in the context of actually modified machine. It communicates with Ansible over well defined JSON protocol, collected from `stdout`. We have two basic forms - error:

{% highlight javascript %}
{ "failed": true, "msg": "Details of the error." }
{% endhighlight %}

And success:

{% highlight javascript %}
{ "changed": true, ... }
{% endhighlight %}

If we want to communicate new facts, which can be used in further tasks (e.g. as an argument for conditional statements) we can add it inside `ansible_facts`:

{% highlight javascript %}
{ "changed": true, "ansible_facts": { "new_fact": "new_value" } }
{% endhighlight %}

It is a good practice to prefix your facts with name of the module (for avoiding name conflicts and preserving clean structure).

#### Implementation

Further implementation is pretty straightforward, especially that we're writing a sequential script using all power of Erlang related with [pattern matching](https://github.com/afronski/playground-infrastructure/blob/master/ansible/custom_modules/xml_status_page/xml_status_page#L40), [*HTTP client*](https://github.com/afronski/playground-infrastructure/blob/master/ansible/custom_modules/xml_status_page/xml_status_page#L26) and [flow control](https://github.com/afronski/playground-infrastructure/blob/master/ansible/custom_modules/xml_status_page/xml_status_page#L60).

### Testing with Ansible

When you want to test your module with Ansible, you can set it up from source inside your `virtualenv` and then invoke it by this series of commands:

{% highlight bash %}
playground $ git clone git://github.com/ansible/ansible.git --recursive
playground $ source ansible/hacking/env-setup
playground $ chmod +x ansible/hacking/test-module
playground $ ansible/hacking/test-module
               -m ./xml_status_page
               -a "host=localhost port=9999 xpath=//statistics name=version"
{% endhighlight %}

## Summary

Thanks to the good architecture and separation of concerns in Ansible, we can easily create modules using different languages and techniques, which will be executed on the provisioned host by a runner. There is no cleaner way to separate executor from task implementation, if you can separate it via programming language.

Also nice structure of this tool and ability to combine it with `virtalenv` allows us to use locally modified versions and prepare our own tailored toolboxes, customized to our projects and needs.

## Credits

- [`afronski/playground-infrastructure` - Implementation](https://github.com/afronski/playground-infrastructure/tree/master/ansible/custom_modules/xml_status_page)
- [Developing modules - Ansible documentation](http://docs.ansible.com/ansible/developing_modules.html)
- [Modules Extras - Ansible](https://github.com/ansible/ansible-modules-extras)
- [How to write Ansible module in Clojure? - Kamil Lelonek's blog](https://blog.lelonek.me/how-to-write-ansible-module-in-clojure-5b340df90f5a)
- [`escript` - Erlang Documentation](http://erlang.org/doc/man/escript.html)
- [`xmerl` - Erlang Documentation](http://erlang.org/doc/man/xmerl.html)
- [`httpc` - Erlang Documentation](http://erlang.org/doc/man/httpc.html)
