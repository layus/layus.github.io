---
title: A Tour of the Build Systems Galaxy
author: Guillaume Maudoux
tags: ["build systems"]
published: 2021-01-22
---

In the [last episode], I presented a concise definition of build systems.
I designed it so that it includes as many approaches as possible to the
problem. I fantasized this episode as a trip along the border drawn by
this definition. The idea was to explore curious build systems, from
those that barely fit the definition to the mainstream ones, passing by
little known outsiders and niche designs.

[last episode]: ./2020-11-09-what-are-build-systems.html

As it turns out, the article was already long enough after exploring
quite a few implementations. It turns out that in my quest to start with
simple tools, they all happened to share a common feature: the build
specification amounts to a single script. None of the build systems
presented here require an explicit list of tasks inputs, nor any other
kind of upfront information about the commands that have to be executed.
For now, let's refer to these as script-based build systems.

The name "script-based" build systems does not exist yet. It is
introduced here for convenience. They are sometimes called "forward"
build systems, but I prefer the more technical and evocative term of
"imperative" build systems, for reasons I elaborate at the end of this
article.

So here is a tour of all the *<abbr title="choose one !">{script-based, forward,
imperative}</abbr>* build systems that I am aware of.

# Scripting build systems

## Down to the basics

The minimal build system often comes as a surprise: it is a single
script. Either bash, bat, or any scripting language. Wikipedia's
definition for scripting languages fits perfectly in [our
definition](./2020-11-09-what-are-build-systems.html) of build systems.
We reproduce it here for it is terser than the ECMAScript spec
referenced as a source:

> A scripting or script language is a programming language for a special
> run-time environment that automates the execution of
> tasks;[^ECMAScript] the tasks could alternatively
> be executed one-by-one by a human operator.
>
> ::: {.cite}
> -- [Scripting
> Language](https://en.wikipedia.org/wiki/Scripting_language), in
> <cite>Wikipedia</cite>
> :::
>
> ::: {style="clear: both;"}
> :::

[^ECMAScript]: https://tc39.es/ecma262/#sec-overview

Everything is in there. A scripting language *automates* the *execution*
of *tasks*. They can definitely be used as build systems, provided that
the tasks are used to process and produce some information.

Here is a trivial build system for a no less trivial application written
in C.

```bash
#! /usr/bin/env bash
gcc lib.c -o lib.o
gcc app.c -o app.o
gcc app.o lib.o -lm -o app
```

This build system works correctly, but leaves a lot of room for
optimisation. It is nonetheless a perfectly valid build system. For this
small project it is easy to setup, easy to maintain, and easy to install
as the script language used is assumed to be installed by default in the
developer environment.

## Dedicated tasks runners

This basic ability of running a sequence of tasks is already interesting
enough in itself that it received it's own dedicated name: a task
runner. The best examples are Grunt and Gulp that both target JavaScript
code bases. Here is a sample Gruntfile, the name of Grunt configuration
files, from [the project official
website](https://gruntjs.com/sample-gruntfile).

```js
module.exports = function(grunt) {

  grunt.initConfig({
    jshint: {
      files: ['Gruntfile.js', 'src/**/*.js', 'test/**/*.js'],
    }
  });

  grunt.loadNpmTasks('grunt-contrib-jshint');

  grunt.registerTask('default', ['jshint']);

};
```

Task runners elaborate on scripts by providing specific support for
build systems. Grunt and Gulp come with progress report during
execution, and convenient tools for declaring the tasks. As the example
above suggests, generic tasks can be shared and reused in several
projects. They can be further specialized with configuration options. Of
course custom, hand-written tasks can still be described. For
open-source build systems, the set of available tasks, modules or
plugins typically grows with the size of the community using it.

These tools also have an implicit understanding of what task outputs and inputs
are. Files in this case. But hey will nevertheless support tasks with no input,
nor output. For example a task that starts, restarts or stop a web server works
fine[^gruntfile]. Tests also fit this model as tasks without output. A failure
of these halts the build, just as any other would.

[^gruntfile]: https://gruntjs.com/sample-gruntfile

The order of the tasks must still be specified manually, as they are
executed in the order in which they are specified. Grunt users must
ensure that all the intermediate files are referenced correctly.

Finally, tasks are named, which makes it easy to construct a tree of
tasks, or run only a named subset of them. The example only has a
`'default'` task to run, but bigger projects can have many more and
compose them.

While these tools use none of the advanced algorithms we will describe
later, they fit their environment. In the context of JavaScript web
development there is no long compilation phase. But files are often
required to go through several transformation steps (concatenation,
minification, uglyfication, compression) and then several operations
need to be automated (testing, deployment, restarting of server,
refreshing ow browser pages, etc.). These tasks are hard if not
impossible to capture in the strict models used by other tools.

Because tasks are quick, it is okay to run them unconditionally. And
because it is hard to automatically determine which one can correctly be
skipped, this is often the only safe option.

## When you are already up-to-date

Memoize and Fabricate are our two next curiosities in the galaxy of
build systems. Conceptually, they are the same thing because Fabricate
presents itself as a rewrite of Memoize that also supports
Windows[^fabricate]. They speed up build scripts
by skipping tasks whose inputs are unchanged since their previous
execution. The specificity of these tools is that they trace the
execution of the tasks at a low-level to extract all file accesses. The
method is thus applicable to a wide range of tasks for a wide range of
projects independently of the language or tools used.

[^fabricate]: "It was inspired by Bill McCloskey's make replacement, memoize,
  but fabricate works on Windows as well as Linux." in Fabricate's
  [README.md](https://github.com/brushtechnology/fabricate#user-content-fabricate)

Technically, two discoveries happen on a task execution. The set of
dependencies are learned, and their content is fingerprinted. Note that
the order of the tasks is still fixed by the script. Tasks can only be
skipped, but not reordered by the build system. This caching technique
enable incremental building, where only a subset of the tasks are
executed to "refresh" a build. The build systems presented before did
not have this property.

### Scripts and command wrappers

Fabricate and memoize have two related modes of execution. They can be
invoked as single command wrappers, and as script libraries. In wrapper
mode, every command invocation must be prefixed by the wrapper to take
effect.

```sh
#!/bin/sh
memoize.py gcc -c file1.c
memoize.py gcc -c file2.c
memoize.py gcc -o program file1.o file2.o
```

The script mode comes as a convenient way to use the wrapper when
programming in a language for which bindings exists, or directly in the
language in which the wrapper tool has been written. In the case of
these two tools, python is the only choice.

```python
from fabricate import *

sources = ['program', 'util']

def build():
    compile()
    link()

def compile():
    for source in sources:
        run('gcc', '-c', source+'.c')

def link():
    objects = [s+'.o' for s in sources]
    run('gcc', '-o', 'program', objects)

def clean():
    autoclean()

main()
```

The core feature is the `run()` command that executes the given system
command with the wrapper in place. But the script mode can also
alleviate boilerplate as is the case in the above example with a
`main()` function that automatically derives available targets from
functions names, and support for cleaning generated files with
`autoclean()`

Truth be told, there is no essential difference between the script and
wrapper mode because both need to identify commands to be executed with
the wrapper, and the script mode does no much more than deferring to the
same logic as the wrapper mode.

## Command wrappers for everything

More generally, any command wrapper can be seen as minimalist build
system handling one command, and can be further threaded into build
scripts to profit from their features. As is the case with ccache and
sccache.

The name CCache stands for Compiler Cache. A wrapper able to perform
caching of compiler invocations. It best supports gcc, clang and cuda
and thus targets mostly C/C++ compilation tasks. SCCache is a separate
tool to support sharing of those caches across the network. The name
stands for Shared CCache.

These tools elaborate on the idea of Fabricate by caching build results
instead of tracking if existent files are up-to-date. This allows to
substitute the output of tasks that were part of older executions of the
build system. With enough care, these outputs can be shared across build
directories, across users on the same machine, or even across the
network.

There exists other commands wrappers like distcc, that distributes the
wrapped commands within a cluster of machines to speed up the
compilation. Icecream does the same with a central scheduling server.

As a final command wrapper, we should mention recc which brings together
remote (aka distributed) execution and remote caching.

Command wrappers allow to gain extra features with minimal changes to
existing build specifications. They can be used as is, or as an
intermediate step to estimate the potential gains of their respective
features before migrating to an advanced build system, potentially not
script-based.

## Can we get faster

All the build systems seen up to now execute tasks in the script order.
This does not prevent some parallelism to take place when the script is
thus written, but it forces upon the build system users the necessary
verification that the script is written in the correct order.

Rattle tries to bypass this limitation by introducing speculative
execution. Rattle is an experimental build system based on tracing
commands execution to skip identical invocations (like fabricate) and
provide caching (like a generalized ccache for any system command).
Rattles tries to compete with advanced build systems while retaining the
simplicity of script-based build descriptions.

To further speed up linear build scripts, Rattle takes the risk of
wasting ressources and speculates on future commands to execute them
anticipatively. Advantages and pitfalls with speculation are well known
in computer science. The impact in the case of Rattle has been studied
in the introducing paper [@spall-2020].

The correctness of anticipated executions is guaranteed by the
system-level tracing on the commands, which ensures accurate information
on inputs and outputs of the commands and allow to detect changes to
inputs of eagerly executed commands.

By tracing execution, the build system may however discover issues that
where not apparent during the execution of the build script. The
implementors of Rattle discovered build specification containing two
commands writing to the same same file. This shows how collected
information about the build specification can discover and help enforce
correct results.

# Script based build systems, and beyond

From the bare execution of a series of commands in an executable script
to the speculative execution of cached tasks execution, we see that a
build script can get various improvements. First performance wise, with
incremental builds, caching, distributed caching and remote execution.
Also with respect to usability with time estimates (the so called
progress bars) and simplified tasks description. And finally regarding
correctness issues when race conditions and outputs variations are made
visible by the tooling.

If we step back a bit, to look at the full list of build systems
presented here, we observe they are not the usual tools one would
associate with build systems. Some did not gain traction, some are still
experimental, some are better described by their specific function than
as build systems on their own. Their lower common denominator, the build
script, is usually conceived as a draft waiting to be replaced by a
proper build system. They are however perfectly valid build systems.

What they have in common is the way they encode the tasks that must be
executed: as a sequence of instructions. They describe the commands to
be executed in an imperative way, as opposed to the declarative paradigm
where commands are described as data. In the imperative script style,
the commands cannot be listed without executing the script. And the
build system requires no a priori information on the tasks to perform
correctly.\
It is precisely this lack of information on the tasks that limits the
optimisations that can be successfully applied during the execution of
the task set. This is why one needs to ressort to speculative execution
to take advantage of parallelism beyond what the script prescribes.

In the literature, the term "forward build system" is also used to
describe these imperative build systems. The term seems to have appeared
within Shake source code, but we could find no definition of it. A
possible interpretation is that declarative build systems configuration
allow the build system to work backward from the final goals and build
the dependencies transitively, while imperative build systems
configurations only work in one direction, forward, the normal direction
of execution for scripts.

The interest of imperative build systems reside in their simplicity.
There is no need to learn a specification (aka configuration) language.
There is no need to explicit or even know the dependencies of the tasks.
All that is needed is to reproduce the set of commands that where
typically entered manually.

Again, the trade-off resides at the information you are willing to
encode. Imperative scripts encode less exploitable information about the
tasks, which makes them shorter and easier to write. But as the software
project becomes bigger and bigger, providing more information enables
build systems to use more efficient optimisations, and to provide more
correctness guarantees on the final result. At the expense of more
maintenance on the build specification.

We can only surmise that it is because imperative build systems tend to
be used in small projects that they did not receive a lot of attention.
Despite their relative discretion, they are numerous and diverse, and
enabled this introduction to techniques and algorithms specific to build
systems in general.

