---
title: Compose something
tags: ["build systems"]
---

<!-- 
We can use composition of build systems to trigger the question of how to allow tasks to reuse internal tasks.

In the graph of topics, we have
1. dependencies (prerequisite relations)
2. motivations (topics that motivate each other, even tough a topic per se is not a motivation)
3. related ideas on a given broder topic, or _lens_. (i.e. build systems kinds, storage, cache, etc).
   these can be seen as axes along which most build system should be positionable.


testing... redo it.



What makes a correct imperative build system ?
It is correct iff it reaches 


Problem 1: make a good build system based on a valid set of tasks
Problem 2: make a good build system based on a random/malicious set of tasks (how to catch invalid tasks)
Problem 3: what are the requirements on the tasks ?

Why do we care ? It is because composition introduces nasty tasks (self updating = stateful, unknown outputs, etc.)
To say what can compose with what, we must see the limit of acceptability on the build system.

Do we want to change build systems for composability, or restrict composability to compatible build systems ?

Research question: Do we want to.

IDEA: Start with existing compositions:
- nix + any / some
- tup @ firefox
- any + rust
- recursive make
- ./configure; make; make install


For composition, the weakest the build system the easier, but we get no guarantees on the result.
There are:
 - incompatibilities
 - weakening
 - preservation; and
 - increase
 of properties over time.




A model for tasks:
    1. Specs for
        a. inputs
        b. outputs
        c. state ?
        d. compatible outputs(.d files) ?
        e. access to OS features (sandboxing, networking, )

    2. A model of computing ?

A model for build systems:
    1. Specs for running tasks.
        a. (dis)allowed inputs
        b. (dis)allowed outputs
        c. required, depended upon, inputs.

-->











































# Existing compositions

Composition of build systems poses a lot of tricky questions.
But foremost, we must wonder why such compositions are needed, or what they could be useful for.
If it is not broken, don't fix it.

These compositions come in handy when handling multiple
languages in polyglot projects where these languages may come with their own
build system, packaging system and dependency manager. Think cargo for rust, npm for javascript and setuptools/pip/pypi for python.
It is also important for meta-projects that aggregate multiple software projects together, as is the case with nix.
<!-- and curiously not the case in the android ecosystem. -->



## Make & gcc -MD



## Cargo and <widcard>
## <wildcard> and Cargo

## ./configure; make; make install

## Nested build systems like stroll, with tracking, and why tup cannot be used inside.



<!-- i.e.; just rerun it -->


