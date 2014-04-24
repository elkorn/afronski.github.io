---
layout: post
title: Gibberish Game
date: 2014-04-24T23:00:00Z-02:00
---

# Gibberish Game

Sometimes you cannot properly name the actual thing. Sometimes you exactly know how to name it, but it will introduce ambiguity and confusion. *How to avoid overloading certain terms and assign proper name to the actual definition?*

### What?

Answer how to do it is related with a technique from *domain driven design* process called *modeling whirlpool*. This game helps *modeler* choose and adjust a word to the description which *domain expert* provides.

### When?

You definitely should use this technique when the actual term cannot be safely used in unambiguous way or it is really overloaded with meanings e.g. couple of aggregates can be associated with it. By defining different, simple, temporary and not meaningful name to the definition we are enforcing our mind to use different parts of brain in order to *discover* the actual meaning. It is a way of jumping out just for a while from the scientific and analytic mindset to the more creative one.

### How?

- Do not name model elements too early.
- Name it at first as *`foo`*/*`bar`*/*`blah`*/*`ble`*/etc. and describe its rules and features.
  - Proper and what is even more important - descriptive and clear name will automatically appear after a while.
- In the next iteration of describing things choose different word.
  - Why? Because your brain already learned what the previous word means.
- If *`foo`*-ish names sounds offensive and are not appropriate for the audience, you can use Greek letters instead.
  - *`Alpha`*/*`Kappa`*/*`Psi`*/etc. sounds more professional :stuck_out_tongue_closed_eyes:.

# References

1. [*The Gibberish Game*](http://goodenoughsoftware.net/2012/02/28/the-gibberish-game/)
2. [*The Context Game*](http://goodenoughsoftware.net/2012/02/29/the-context-game/)
3. [*Model jest wszystkim czego potrzebujesz* (PL)](https://www.youtube.com/watch?v=iaLeKHbspLg)