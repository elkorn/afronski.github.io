---
layout: post
title: How to store and examine custom business metrics?
date: 2015-01-24T22:00:00Z-02:00
---

# How to store and examine custom business metrics?

Recently, I spent some time looking for an ideal provider, which will deliver storage and graphing capabilities for our custom business metrics. Each growing business need to track your `KPIs` but also other values, on the internal and much more granular level. It was not an easy job to do, because we have got very specific requirements.

## Requirements

- We need *schemaless* storage with support for JSON.
- We need a possibility to query, group, filter by plain fields and nested object structure.
- We need to deliver multiple and custom data series, represented as business metrics.
- We need to deliver them over `HTTPS`.
- We need a possibility to create graphs, tables and dashboards.
- We need at most one hour delay when displaying graphs from actual data.

Obviously, it looks like too many requirements for one tool, especially that we were focused on the *SaaS* solutions - for obvious reasons we don't want to host anything by ourselves. Moreover, it will be nice to have a dedicated *Node.js* client available for such service.

## Services

We have evaluated numerous solutions (*around 25 providers*) and we have found these mentioned below as these which match our requirements:

- [Keen IO](https://keen.io/) - That is the most complete solution for us. It has everything (even a nice *Node.js* client), however it does not host dashboards and charts in their place. It is a very nice storage and API only. Fortunately, there is a [project](http://keen.github.io/dashboards/) which provides nice, *Bootstrap* powered, templates for [Keen IO](https://keen.io). Unfortunately you have to host them on your own.
- [DataHero](https://datahero.com/) - It is the most impressive solution, with amazing charting abilities, but without any kind of storage and API for pushing metrics. They are specializing in visualization and data analysis. It is possible to integrate [Keen IO](https://keen.io) with it, however you can schedule update of your data only once a day. Second possibility is to use the *Google Drive Spreadsheet* in order to achieve real-time integration for your dashboards, but it feels like a *hack*.
- [StatsMix](https://statsmix.com/) - Very nice complete solution, which has dashboards, metrics storage. Unfortunately bugs related with `CSV` export, lack of JSON metadata filtering and grouping and plenty of UI glitches destroy the overall experience.
- [New Relic Insights](http://newrelic.com/insights) - Again, nice and complete solution but without support for JSON metadata (lack of filtering, storage and grouping). If you would like to store plain, flat structure it is a very nice solution to do that.
- [Segment](https://segment.com/) - It is a nice tool, but it is only a *black-box* which eases integration across multiple services. You can basically insert data into [Segment](https://segment.com) (with a nice *Node.js* client) and forward them automatically to the other integrated services (like *Google Analytics*, [Keen IO](https://keen.io) and so on). They do not have dashboards and export API.
- [Librato](https://www.librato.com/) - Again, very nice and complete solution without support for nested JSON objects. If you need to store and display graphs from only flat and plain metrics it is a nice tool to check.

## Summary

Our choice focused on [Keen IO](https://keen.io) integrated via [Segment](https://segment.com). It has an API and JavaScript library for drawing charts (which uses *Google Charts* underneath), however it does not host your charts and dashboards in one place, so it is not a complete solution. So why we chosen that one? Simply because of that the rest of solutions have far more problems than [Keen IO](https://keen.io) with our requirements.
