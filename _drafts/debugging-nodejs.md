---
layout: post
title: Debugging Node.js
---

No dtrace, no Smart OS - only gdb and:
- ulimit -c
- echo '/tmp/core_%e.%p' | sudo tee /proc/sys/kernel/core_pattern
- node-gyp, g++ and gcc '-g' flag.
- gdb /usr/bin/node core-dump-file

In other words - describe whole voodoo ;).