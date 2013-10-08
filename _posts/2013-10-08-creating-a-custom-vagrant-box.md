---
layout: post
title: Creating a custom Vagrant box
---

# Creating a custom Vagrant box

![Vagrant Logo](/assets/VagrantLogo.png)

*vagrant - a person without a settled home or regular work who wanders from place to place and lives by begging.*

# It is not a common path...

Do not start creation a new box from scratch without checking both of these sites:
- [vagrantbox.es](http://www.vagrantbox.es/)
- [veewee](https://github.com/jedi4ever/veewee)

In 99% of cases, one page will already contain the solution, which is a needed box. Otherwise we have to roll up one's sleeves and go through whole process. This post will contain almost all directions for creating fresh box.

And last but not least: all recipes provided in this article are valid for Vagrant in version 1.2 and above.

# ... but sometimes necessary...

#### Install Vagrant

I've assumed that you already have Vagrant - if not, go to [downloads](http://downloads.vagrantup.com/) page or install it by using package manager from your Linux distribution.

#### Virtual Machine in VirtualBox

At first we have to prepare virtual machine with specified amount of RAM, number of virtual processors, virtual hard disk image and rest of virtualized hardware. Remember VM name - you will need it at the end :wink:.

One important thing related with network - create only one network adapter and set it for using NAT. At last setup port forwarding like presented below:

![Port forwarding setup for test purposes](/assets/VirtualBoxPortForwarding.png)

#### Installation

Then grab your favourite *Linux distribution* / *Unix flavour* ISO image and proceed with installation process (no Windows because right now Vagrant doesn't support this operating system as a guest).

This is the easiest step in whole guide :grin:.

#### User management

After installation create new user account:

{% highlight bash %}
# Create user 'vagrant'.
root> useradd vagrant

# Setup password.
root> passwd vagrant
{% endhighlight %}

And add him to proper groups:

{% highlight bash %}
root> usermod -aG users,wheel vagrant
{% endhighlight %}

And final step here - disable password for `sudo` command in `wheel` group:

{% highlight bash %}
root> visudo

# Uncomment or add line like below:
%wheel ALL=(ALL) NOPASSWD: ALL
{% endhighlight %}

#### SSH

At first open port 22 on your machine, then get this [public key](https://raw.github.com/mitchellh/vagrant/master/keys/vagrant.pub) and put it in the file:

{% highlight bash %}
root> curl https://raw.github.com/.../vagrant.pub > /etc/ssh/authorized_keys
root> chown root:root /etc/ssh/authorized_keys
{% endhighlight %}

Then setup your `sshd_config` file like presented below:

{% highlight bash %}
# Uncomment these lines:
Port 22
AddressFamily any
ListenAddress 0.0.0.0
ListenAddress ::
PermitRootLogin yes

# Also uncomment and change these:
AuthorizedKeysFile /etc/ssh/authorized_keys
PasswordAuthentication no
{% endhighlight %}

Then you can verify your settings by command:

{% highlight bash %}
~ $ ssh -i ~/.vagrant.d/insecure_private_key vagrant@localhost -p 2222
{% endhighlight %}

If there are no errors you're ready to the next step.

In some cases you'll receive error similar to this one (**Ubuntu / RedHat compatible tip**):

{% highlight bash %}
~ $ ssh -i ~/.vagrant.d/insecure_private_key vagrant@localhost -p 2222

Permission denied (publickey,gssapi-keyex,gssapi-with-mic)
{% endhighlight %}

In this case you need to fix labels for file with authorized keys:
{% highlight bash %}
~ $ cd /etc/ssh/
~ $ ls -laZ                     # Checking labels.
~ $ restorecon -r -vv .         # Fixing them.
{% endhighlight %}

#### Finishing touches

Unfortunately our `sudo` has one big disadvantage - it won't work without a real TTY and as you probably guessed - vagrant doesn't use it. So we have to disable this setting in `sudoers` file:

{% highlight bash %}
~ $ sudo visudo

# Comment or remove line like below:
Default requiretty
{% endhighlight %}

Just before last step, please remove all previously forwarded ports for your virtual machine in VirtualBox.

#### Gift packing!

We still have to do the last step - packaging:

{% highlight bash %}
~ $ vagrant package --name <VirtualBox VM name> --output FreshAndHotVagrant.box
{% endhighlight %}

The result is a finished box, which we can add to our list:

{% highlight bash %}
~ $ vagrant box add <internal box name> FreshAndHotVagrant.box
{% endhighlight %}

Then feel free to use it inside `Vagrantfile`:

{% highlight ruby %}
Vagrant.configure("2") do |config|
  config.vm.box = "<internal box name>"
end
{% endhighlight %}

# Links and Image credits

1. [Original link to Vagrant logo](http://www.hashicorp.com/images/blog/a-new-look-for-vagrant/logo_wide-cab47086.png)
2. [Vagrant documentation](http://docs.vagrantup.com/v2/)