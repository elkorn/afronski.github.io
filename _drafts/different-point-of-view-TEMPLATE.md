---
layout: post
title: Different Point of View - [TEMPLATE]
date: 2016-01-01T16:00+0200
categories:
  - different-point-of-view
tags:
  - series
  - interactive
  - visualization
published: false
---

# Different Point of View - [TEMPLATE]

<quote class="disclaimer">*TODO*</quote>

# [TEMPLATE]

## What's next?

## Credits

- [TEMPLATE]

---

## Tools

  - https://github.com/pyramation/LaTeX2HTML5
  - https://github.com/worrydream/Tangle
  - https://github.com/bookiza/bookiza

## Materials

  - https://bost.ocks.org/mike/algorithms/
  - http://biecek.pl/Eseje/indexDane.html
  - http://www.nytimes.com/interactive/2012/05/17/business/dealbook/how-the-facebook-offering-compares.html
  - https://jackschaedler.github.io/circles-sines-signals/
  - https://blog.risingstack.com/history-of-node-js/
  - http://world.mathigon.org/Sequences
  - http://setosa.io/ev/
  - http://harry.me/blog/2014/12/27/neat-algorithms-paxos/
  - https://github.com/yang-wei/elmflux

## Current topic

- Percentiles
  - Materials:
    - My book from studies.
    - https://en.wikipedia.org/wiki/Percentile
    - http://greenteapress.com/thinkstats/thinkstats.pdf
  - Observations:
    - Different definitions, different results for smaller population.
    - Decile, Quantile, Percentile.
      - Difference between Percentage and Percentile.
      - Visualization on normal distribution.
        - What about other distributions?
        - Visualization together: https://flowingdata.com/2012/05/15/how-to-visualize-and-compare-distributions/
          - Shiny + MathJax?
      - Z-score and percentiles relationship.
        - How to calculate percentile from z-score?
      - Median measures the better half only.
      - Fastest case? Only viable for benchmarks, but it doesn't say anything else.
      - Why averaging percentiles is wrong?
    - Population, stupid!

- Article flow:
  - Show example of latency chart.
  - Prepare:
    1. Tangle.js + D3.js (with average, median).
    2. Then apply min and max.
    3. Oh shit, what happened?
    4. What is an average, what is hiding.
  - Let's look on the distribution, show directly the tail latency.
    - Population.
  - So, what is percentile? Decile? Quantile? Median?
    - Different between percentage and percentile.
    - Why it is crucial in this case?
    - Why averaging percentiles is wrong?
  - Difference between latency chart and normal distribution.

## Possible future topics

- Latency - distribution, percentiles, calculations
  - https://www.youtube.com/watch?v=9MKY4KypBzg
  - Latency distribution mirrors requirements.
  - Only percentile chart shows the exact behavior.
  - For 99.999% percentile in normal distribution you should have 4.5 sigmas (or
    7 if you want to be really loose) deviation.
    - Latency is not a normal distribution!
    - But that's good - if it was a normal distribution three numbers will be
      enough to describe it (mean, std. deviation).
      - Quick mental experiment: How random, Gaussian and constant distribution
        look on the percentile chart?
        - It helps thinking about the system behavior!
          - When there is a stalling behavior - the chart is not smooth, there
            are jumps, sharp edges. Stalling means - system stopped and certain
            requests are slowed down.
            - Scheduler, GC ...
          - Similarly for queuing - there will be a "staircase" chart for each
            burst which happen after first holdup.
            - Waiting for disk queue, networking ...
          - Multi-modal distribution has a step function on the percentile chart.
            - Hiccups...
        - Generator: https://github.com/giltene/GilExamples/blob/master/examples/src/main/java/HistogramGenerator.java
    - Profiler will not show you the 99.999% percentile, because it is not your
      hot code. It won't hit the profiler.
    - Latency does not live in the vacuum.
    - Sustainable throughput means that you achieve high throughput when safely
      maintaining service levels.

- Boids

- Amdahl's Law
- Universal Scalability Law

- Queuing Theory
- Little's Law
  - CPU load averages calculation and related topic to queuing theory.

- NTP and Skewed Clocks

- Distributed Locking

- Networking:
  - Three-way handshake (TCP).
  - Congestion (TCP).

- Distributed Systems:
  - [The Byzantine Generals Problem - Lamport, Shostak, Pease (Microsoft Research)](http://research.microsoft.com/en-us/um/people/lamport/pubs/byz.pdf)
  - Two Generals Paradox.
    - [Lecture Notes in Computer Science - Gray (Microsoft Research, starting on page 465)](http://research.microsoft.com/en-us/um/people/gray/papers/DBOS.pdf)

- ASN.1
  - Old useful technologies: http://ttsiodras.github.io/asn1.html

- UNIX tools:
  - How they work underneath?

- Erlang:
  - Virtual Machine visualization.

- AWS:
  - Visualization and associations.
