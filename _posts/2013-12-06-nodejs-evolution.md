---
layout: post
title: Node.js evolution
date: 2013-12-06T18:15+0100
tags:
  - nodejs
  - community
  - evolution
---

# Node.js evolution

![Node.js logo](/assets/NodeJsLogo.png)

*Node.js* in blessed, *enterprise* version `1.0` comes faster then you might have expected (look at the first link in references - Isaac said that `0.12` is the last version before the final release). What will the future bring us?

Bert Belder's talk on LXJS 2013 gives us a small overview about what is to come. It obviously has a provocative first slide and title, but the point still stands - what should we expect from *Node.js `2.0`* ?

# Tasks

One nifty concept, called *Asynchronous Tasks*, was presented in the talk.

It was presented as an asynchronous `try...catch` block without changing the language syntax and semantics. Let's look at the initial representation:

{% highlight javascript linenos%}
task.create(function concatTwoFilesAndThenSomeTask() {

  fs.readFile("some/template", "utf8", function (error, file1) {

    if (error) {
      throw error;
    }

    fs.readFile("something/else", "utf8", function (error, file2) {

      if (error) {
        throw error;
      }

      var data = file1 + "--" + file2;

      data = data.replace(/foo/g, "bar");
      return data;
    });

  });

}).setCallback(function (error, result) {

  if (error) {
    throw error;
  }

  // Do something with the result.
});
{% endhighlight %}

Of course it is a simple concept, which ignores many thing e.g. `EventEmitter`. But it also gives us many things e.g. meaningful stack traces with asynchronous operations or handling errors of composite asynchronous operations in one place.

I know that there exist modules which can do that (like [async](https://github.com/caolan/async)), but having it built into the platform seems very interesting and brings other capabilities (e.g. under-the-hood optimization) which the additional modules do not have.

The whole concept has evolved into something called by Bert a `domains2` module. You can find details in references.

# What else?

We have to remember that the world will not stay in place and will evolve. And *Node.js* in the blessed, *enterprise* version has to evolve too. The owners are the pushing platform in the right direction and I totally agree that *Node.js* should have a thin core as it has right now. However, there are definitely many things which can land in it.

How about:

- Supporting new protocols:
  - [HTTP 2.0](http://tools.ietf.org/html/draft-ietf-httpbis-http2-04)
  - [SPDY](http://tools.ietf.org/html/draft-ietf-httpbis-http2-00)
  - [STUN](http://tools.ietf.org/html/rfc5389) and [TURN](http://tools.ietf.org/html/rfc5766)
- Supporting old ones, as well:
  - [SNMP](http://www.ietf.org/rfc/rfc1157.txt)
- Adding more load balancing algorithms to the *`cluster`* module ([Round-Robin was recently added in `0.12`](http://strongloop.com/strongblog/whats-new-in-node-js-v0-12-cluster-round-robin-load-balancing/)).
- Adding supervision mechanisms (like the ones we have in [Erlang and OTP](http://www.erlang.org/doc/design_principles/sup_princ.html))
- Supporting mesh topology (like in [this module](https://github.com/dominictarr/scuttlebutt)).
- Solving problems with *`NPM`* and keeping it *awesome* (by providing [mirrors](http://npmjs.eu/) or [scaling it](https://scalenpm.org/) and [decentralizing](http://blog.nodejs.org/2013/11/26/npm-post-mortem/)).

The reported enhancements are just the tip of the iceberg and even you can expand this list. I encourage you to subscribe to *Node.js* [mailing list](https://groups.google.com/forum/#!forum/nodejs) and then observe, learn, share ideas and experience, discuss.

I encourage you to be a conscious and active member of the community, maybe involve in some initiatives mentioned above, like mirroring or scaling the *`NPM`* etc.

The future looks promising only with the platform evolving with it - and evolution depends only on us.

# References

1. [Isaac Shlueter - The Road to Node.js v1.0](http://www.youtube.com/watch?v=82hJbjqbIt4)
2. [Bert Belder - Node.js 2.0 (LXJS 2013)](http://www.youtube.com/watch?v=QnO6Uut4Ao8)
3. [`domains2`](https://gist.github.com/piscisaureus/7454729)
4. [Node.js Logo](http://blog.nodejs.org/2011/07/11/evolving-the-node-js-brand/)
