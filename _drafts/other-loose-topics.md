---
layout: post
title: [TEMPLATE]
date: 2016-01-01T16:00+0200
tags:
  - other
---

# [TEMPLATE]

### Introduction

### Credits

[TEMPLATE]

---
# Ansible vs. Chef

- Ansible - agent-less, SSH based, Python.
  - YAML-based, modules in any language - communicating via JSON, running them at the end system.
- Chef - agent, server-client architecture, Ruby.
  - Ruby, full blown framework, extending only with that language.

# V8 and compilation cache

- V8, Compilation Cache, Function.prototype.toString (it returns everything),
  trick with UID in the comment of function body (for distinguishing JITed
  functions) and why functions with huge comments are de-optimized by V8 (they
  are too big for cache).

# Maintainability hell on your own wish

- No standards, no common idioms. Everyone is solving the same problem in multiple different ways. Do you want an example - let's talk about asynchronous flows? Callbacks, promises, generators and incoming standard `async` / `await`. Each one is significantly different.
- Multiple ways of doing things is refreshing and really flexible, but it is unmaintainable. Huge cognitive overhead, when you're doing standard operational stuff after a long period of time without touching it.
- LTS? It happens recently, I've seen enterprise class applications, used in places so critical, that it is hard to imagine written in `0.10` and migration to `0.12` is a huge burden, an almost year of testing campaign and adjusting stuff (and often rewriting some core modules, because there were no common ABI wrapper for V8 at that time and native modules are incompatible with that version). Did I say that we've have two major releases since then?
- No standards library? That's why people are incorporating huge pile of everything on the start.
  - If you want to deliver something you'll not reinvent the wheel, and obviously - writing `leftPad` on your own, seeking common implementation, even collecting on your own pieces of scripts is still at least yak shaving operation. If you want to get job done, you're adding huge pile of everything at the start. It is still visible in other places - e.g. Java people before 8th version were attaching Guava (and sometimes Guice) at the start of every project.
- Maintainability? Nice joke. Did you try to collect GC pauses in Node.js (really standard thing for monitoring purposes in your environment)? If yes, you know that it is impossible without incorporating into your codebase C++ add-on, which makes your whole single-threaded server potentially unstable. It is like doing C NIFs in Erlang, but there you know what you're doing and you're doing it because of performance reasons.
- Standard deployment? You need to work on that by your own. And that is not a particularly hard problem, but again - it introduces cognitive overload.
- The `left-pad` issue is a partially "political" thing and partially misunderstanding of OSS ecosystem. There is no "my" in OSS, especially for widely used packages. Ego and hubris is a wrong thing to follow there.
- Whole problem with maintainability is connected with huge amount of people, huge variation of perspectives and how things should be done. That's not a wrong thing, if you consider Node.js as a prototyping platform, e.g. for start-ups or growing businesses. Considering low-entry barrier for programming language and ease of finding developers it is an ideal language for such ecosystem.
  - If you ask me about solid platform, used in the long term, that has to be stable, available and maintainable over the years (and I'm talking about 5+ years of maintenance) I would not consider this a good choice. More - I'd consider it as a shooting yourself in the foot in the long run, with operational nightmare.
- What scares me the most are critical places that I've seen Node.js used. I feel pain of those poor people that'll have to maintain it. Do you want to track GC pauses? You've to add a 3rd party C++ addon in version 0.0.1, which blows up stability of your single-threaded server, because doh - it is hooking up into VM internals and does C++ in a hazardous environment like V8.
- https://twitter.com/knewter/status/713673979429568516

# EHS of Erlang shell - How to safely quit?

- Development and plain shell.
- Releases - Attach vs. connect.
- Remote shell.

http://erlang.org/faq/getting_started.html
http://erlang.org/documentation/doc-5.3.6.13/doc/getting_started/getting_started.html
http://learnyousomeerlang.com/starting-out
http://learnyousomeerlang.com/distribunomicon
http://stackoverflow.com/questions/12926291/erlang-detach-shell-from-node-quit-shell-without-killing-node
http://stefanalfbo.github.io/blog/2013/04/23/erlang-shell-cheat-sheet/
http://erlang.org/doc/man/shell.html
