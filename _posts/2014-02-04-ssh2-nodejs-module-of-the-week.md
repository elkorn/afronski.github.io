---
layout: post
title: ssh2 - Node.js module of the week
date: 2014-02-04T22:00:00Z
---

## `ssh2` - Node.js module of the week

### Introduction

In this series of articles I would like to show interesting, sometimes solid, sometimes just working and really useful *Node.js* modules which helps me in the last week (or earlier). Each post will describe a reason why I chosen this module, an API description or sample usage. It will also describe *health* (in other words - what it supports and answer for question *"Is it actively developed?"*) and *quality* (documentation, tests and overall code quality).

### Description

`ssh2` is a `SSH2` client module written in pure JavaScript.

In this case mentioned module helps me bootstrap the simple tool a while ago. The main goal of this tool is to automate create, download and upload a backup to `SFTP` for *one-of-the-most-popular-services-for-project-management* which in cheapest version does not provide an API for that.

For the first part (*UI automation script* which will create the backup and download an archive) I have used a *`node-chimera`* module (which I will describe in the next blog post) and for second part (*uploading previously downloaded `ZIP` archive* to two different `SFTP` servers) I have used a *`ssh2`* module.

### API

In the most basic case you need only to *`require`* this module and create new connection. It is useful to use *domains* for error handling, but it is not an obligatory practice.

{% highlight javascript linenos%}
var fs = require("fs"),
    util = require("util"),
    domain = require("domain"),

    Connection = require("ssh2"),

    OptionsForSFTP = {
      host: "X.Y.Z.U",
      port: 22,
      username: "backup",
      // password: "password"
      privateKey: fs.readFileSync("/path/to/privateKey")
    };

// ...

function uploadToSftpServer(options, fileName, callback) {
  var connection = new Connection(),
      handler = domain.create();

  handler.on("error", function (error) {
    console.error("Error occurred: %s", error);
    process.exit(-1);
  });

  // Handling "error" event inside domain handler.
  handler.add(connection);

  connection.on("ready", function () {
    connection.sftp(handler.intercept(function (sftp) {
      var providedFileName = util.format("./%s", fileName);

      sftp.fastPut(providedFileName, providedFileName,
                   handler.intercept(connection.end.bind(connection)));
    }));
  });

  connection.on("end", callback);
  connection.connect(options);
}
{% endhighlight %}

When connection is *`ready`* we need to open a *`SFTP`* channel and *`PUT`* file to the server. As you can see in example presented above, authentication via password or private key is a piece of cake and it is handled by module internals.

### Health

Module is under active development, bugs are quickly resolved. Besides bug fixing and merging pull requests author incrementally improves design and code quality.

It is supported on *Node.js* version `0.8` and higher (probably also on `0.11` because it does not have any kind of binary extensions).

### Quality

Unfortunately *module is not* an any kind of reference in terms of quality:
- *It does not have any kind of tests* (unit or even integration tests).
- Documentation is delivered inside single *`README.md`* file in form of source code examples.
- Overall code quality is not too high, module still makes an impression of *hacked* in the hurry.

But it works and it has just a few bugs :wink:.

### References

1. [`ssh2` github page](https://github.com/mscdex/ssh2)