---
layout: post
title: Dependency graphs for humans
---

While working on the Tup backend for mozilla, I stumbled on the super-nice `graph` feature of tup.
Tup is able to produce a dot graph of it's database.
This is well described on [Mike Shal's blog](http://gittup.org/blog/2015/01/12-combining-nodes-in-graphviz/)

![A simple dependency graph]({{ site.baseurl }}/images/2017-09-06-Tup-Graphs/simple.png)

The issue with these graphs is that on real projects, it becomes too big.


I investigated how it could be improved for firefox.


