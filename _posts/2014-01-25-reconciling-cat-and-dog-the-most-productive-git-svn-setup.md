---
layout: post
title: Reconciling cat and dog - the most productive git-svn setup
date: 2014-01-25T22:00:00Z
---

## Reconciling cat and dog - the most productive *`git-svn`* setup

I would like to present productive and effective setup for the *`git-svn`* bridge which removes most of the obstacles and fixes some problems which occur with this *bridged* approach.

### Configuration

Before we start - one remark: if you installed *`git`* on your machine first time ever, please remember about setting up proper user, email and *one-more-thing just for convinience*:

{% highlight bash%}
~ $ git config --global user.name "Your Name Here"
~ $ git config --global user.email "your_email@example.com"
~ $ git config --global credential.helper cache
{% endhighlight %}

### Starting

At first, you need to `clone` (or `checkout` in SVN terminology) repository. You can do in three different ways:
- First way (*2nd line*) checkouts repository with a standard SVN layout (`trunk`, `branches`, `tags`) and associates branches, tags and *`trunk`* with *`git`* *`master`*.
- Second way (*5th line*) checkouts whole repository as it is.
- Third way (*starts in 8th line*) is really an expansion for `git-svn clone`.

{% highlight bash linenos%}
# Standard layout.
~ $ git svn clone -s $SVN_REPOSITORY_ROOT_URL

# Non-standard layout.
~ $ git svn clone $SVN_REPOSITORY_ROOT_URL

# 'git svn clone ...' is basically a shortcut for these commands:
~ $ mkdir REPOSITORY
~ $ cd REPOSITORY
~ $ git svn init $SVN_REPOSITORY_TRUNK_URL .
~ $ git svn fetch -r HEAD
{% endhighlight %}

### Updating and commiting

Besides `pull` and `push` (or `update` and `commit` in SVN terminology) everything works like in classical *`git`*.
First line represents *`svn update`* command, second - series of `svn commit` commands with messages collected inside local *`git`* repository.

{% highlight bash linenos%}
~ $ git svn rebase
~ $ git svn dcommit
{% endhighlight %}

### Ignores

If you cannot store your *`.gitignore`* file in the SVN repository you can exclude these files locally:

{% highlight bash%}
~ $ git svn show-ignore >> .git/info/exclude
{% endhighlight %}

But `.gitignore` file can be very convenient - you can easily share exclusions with other members of the team which also use this bridged approach. *Moreover*, this command is really slow for bigger repositories, so that is another advantage of having this file.

### Branches

If your repository does not have standard layout you can still easily associate SVN branch in your *`.git/config`* by adding new *`svn-remote`* section:

{% highlight bash%}
[svn-remote "sample_branch"]
    url = https://svn_repository/branches/sample_branch
    fetch = :refs/remotes/sample_branch
{% endhighlight %}

Then you should run (and *develop the habit of running*):

{% highlight bash%}
~ $ git svn fetch --fetch-all
{% endhighlight %}

As the last step, you need to *`checkout`* new branch:

{% highlight bash%}
~ $ git checkout -b sample_branch remotes/sample_branch
{% endhighlight %}

After that your branch will be available like normal *`git`* branch. Remember that your *`trunk`* is actually a *`git`* *`master`* branch.

### Removing empty directories

Regarding the *`git-svn`* bridge, dangling empty directories are my nightmare. Fortunately, someone thought about it and we can enable deleting empty directories on commit (*globally* via *`git-config`* or *on demand* just for certain commit):

{% highlight bash%}
~ $ git config svn.rmdir true
~ $ git svn dcommit --rmdir
{% endhighlight %}

### Problems

There are still a few problems with the newest SVN clients for such combination. The most annoying is *assertion ... failed* related to the renaming operation. This error looks like this example:

{% highlight bash%}
~ $ git svn dcommit
Committing to http://...
    C      path/to/file/a.js => other/path/to/file/b.js
assertion "svn_fspath__is_canonical(child_fspath)" failed: file "dirent_uri.c", line 2502, function: svn_fspath__skip_ancestor
{% endhighlight %}

At first glance you may think that the only (and the *worst possible* - lost information and mess in history) option is to split your commit into two. Happily, you can perform your commit normally but you need to turn off rename detection:

{% highlight bash%}
~ $ git svn dcommit -C1 -l1
{% endhighlight %}

Unfortunately you will still commit two changes, this time in one batch (as a *deletion the old file* and *creation file with the new name*), instead of just a file rename, so you will still loose information in the process.

To avoid this completely the only option is to downgrade SVN client to the latest version on *`1.7.X`* branch.

### Credits

Big thanks to [@skremiec](https://twitter.com/skremiec) for undeceiving me in some cases and pointing out *`svn.rmdir`* option :wink:.

### References

1. [Manual for `git-svn` on kernel.org](https://www.kernel.org/pub/software/scm/git/docs/git-svn.html)
2. [Chapter about `git-svn` in `Pro Git` book](http://git-scm.com/book/en/Git-and-Other-Systems-Git-and-Subversion)