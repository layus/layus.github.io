---
title: Am I a build system ?
tags: ["build systems"]
bibliography: ../bibliography.bib
published: 2020-11-09
---

<!-- TODO Add a sentence saying: we did not find something usefulll --> 

In the quest of defining what is a build system, we start by looking at existing definitions. We had to cast the net far and wide for there are few formal attempts at defining this widely used concept. Here are the four that we found.

> The build system is the set of build specification files used by the CI
> infrastructure (and developers) to generate project deliverables like
> binaries, libraries or packages [...] from the source code. Moreover, the
> build system automates many other activities, such as test execution and
> sometimes deployment.
>
> A build system typically consists of a configuration layer and a construction
> layer. The configuration layer is used to select which features should be
> compiled and included in the resulting deliverables, as well as which build
> tools (e.g., compilers, interpreters) are necessary to compile those
> features. Once configured, the construction layer is used to specify the
> build tool invocations that are required to generate deliverables from source
> code. Since these build tool invocations are order-dependent [...], a key
> responsibility of the construction layer is to invoke build tools while
> respecting their dependencies.
>
> [...]
>
> <div class="cite"> 
> -- <cite>Bram Adams and Shane McIntosh</cite>, [Modern
> Software Engineering in a Nutshell](http://mcis.polymtl.ca/publications/2016/fose.pdf#page=4).
> </div><div style="clear: both;"/>

<!-- Modern Software Engineering in a Nutshell

The build systems of large, multi-platform software systems are complex and
difficult to understand and maintain. To assist in this regard, research has
proposed tools to visualize and reason about build systems [4, 79, 83].
Furthermore, recent work has begun to explore how build system problems are
being addressed [7, 21, 33, 53, 54].

Hundreds of different build tools exist, written for different programming
languages and using different paradigms. Older programming languages like C and
C++ have file-based build dependencies, where each file is compiled separately
and de- pendencies are declared between individual files. GNU Make is the most
popular file-based build system technology [50]. Java-based systems have
task-based build dependencies, where a compiler or other tool is able to
compile multiple source code files at once. Ant is the prototypical task-based
build system technology. For example, a compile task of an Ant build system
would invoke the Java compiler, which may read several .java files and update
several .class files.

Lifecycle-based build technologies like Maven consider the build system of a
project to have a sequence of standard build activities that together form a
“build lifecycle.” By placing source code files in the right directory, tools
like Maven can automatically execute the right lifecycle build activity.
Finally, build system generators like CMake provide a high- level
domain-specific language to specify a build, which is then transformed
automatically into build files of one of the three other build system
technologies. Often, specification files can be generated for different
operating system platforms to ease the process of developing a cross-platform
build system.

-->

> Build automation is the process of automating the creation of a software
> build and the associated processes including: compiling computer source code
> into binary code, packaging binary code, and running automated tests.
>
> <div class="cite"> 
> -- Build Automation,
> <cite>[Wikipedia](https://en.wikipedia.org/w/index.php?title=Build_automation&oldid=996141938)</cite>
> </div><div style="clear: both;"/>

> Build systems automate the execution of repeatable tasks, at a scale from
> individual users up to large organisations. (p.2)
>
> A build system takes a task description, a target key, and a store, and
> returns a new store in which the target key and all its dependencies have
> up-to-date values. (p.9)
>
> <div class="cite"> 
> -- <cite>Andrey Mokhov, Neil Mitchell and Simon Peyton Jones</cite>, [Build systems à la carte](https://www.microsoft.com/en-us/research/uploads/prod/2018/03/build-systems.pdf)
> </div><div style="clear: both;"/>


> In this paper, a build system is any piece of software that provides
> facilities for constructing and parsing the DAG which represents the
> dependencies among files in a software project. (p.3)
>
> This paper is not focused on the aspect of building software related to
> configuration options as would typically be handled by software such as
> autoconf or kconfig, even though some of these features have been introduced
> into build systems billed as replacements for make (which itself does not
> have such features built-in). When the build system is invoked, it will input
> the DAG and current filesystem state, and output a new DAG and/or files that
> represent the output of the build. (p.3)
>
> <div class="cite"> 
> -- <cite>Mike Shal</cite>, [Build System Rules and
> Algorithms](http://gittup.org/tup/build_system_rules_and_algorithms.pdf#page=3)
> </div><div style="clear: both;"/>

<!--
1. Automation (Nut, wiki, ALC)
2. Tasks/Processes (Nut, wiki,
3. Files, packages, deliverables, software build, keys
4. dependencies, DAG (Tup, ALC, Nut,
5. scale (wiki, ALC) -->

Build systems are a key part in modern software engineering pipeline[nutshell].
They grew organically from the need to encode knowledge about and automate all
the tasks involved in the transformation of software sources into binary
executables, test results and other artifacts that may be needed further down
the development pipeline. The name "build system" itself seems to come from the
idea that programs  need to be built from their sources before finding any use.
But there exists other tools which encounter similar problems and solve them
using comparable algorithms. For example spreadsheets build their cells values
from formulas. And some package mangers can build sets of interdependent
packages from custom description formats.

In our quest to define build systems, we would like to liberally include
disparate software systems provided that their internal design choices can be
compared. We would like to include single bash scripts that are used as build
systems as well as task runners that do not call themselves build systems, and
also various odd, corner case build automation tool such as [fabricate],
[stroll] or [portage].

We identified key concepts pertaining to build systems by reviewing four
definitions found in the literature.

- Build systems serve a purpose of __automation__. They can be trusted in their
  operation and require minimal if any human assistance to deliver their
  result. Good build systems should operate quietly and reliably[^irony].

- Automation outlines the implicit existence of __tasks__ to be automated. A
  build system supervises the execution of a delimited set of tasks. Build
  systems differ in the kind of tasks they expect and handle correctly, but
  they always expect tasks with a limited lifespan. Tools that manage the
  execution of long-standing processes such as web servers fall in the category
  of monitors and services managers. The completion of a build systems signals
  the end of all the managed tasks.

- This is because build systems view tasks as means to and end: producing build
  __artefacts__. Build systems supervise a bipartite graph of build tasks and
  build products, also know as a data flow graph[^Dataflow]. The nature of the
  data produced, and thus also consumed, by build system tasks encompasses env
  variables, strings, full docker images, software packages and files. The
  latter being the first that comes to mind when thinking about canonical build
  systems.

- Tasks are related to each other through __dependencies__ on their
  productions. Build system have the responsibility to enforce and maintain a
  consistent execution order. While most build system expect a partial order,
  some are capable of handling preorders under certain assumptions. From a
  dependency graph perspective it means that most build systems will expect a
  directed acyclic graph of dependencies but that some provide support for
  dependency graphs with cycles.

- In some cases, build systems are not provided with the __dependency graph__
  upfront. To simplify the management of tasks interdependencies, some support
  automatic discovery of that graph dynamically. As much as the underlying
  graph of dependencies may be used as a reasoning tool, it does not have to be
  reified by build systems to execute the tasks.

- Build systems __performance__, particularly at large scale, may be important
  in certain contexts. We think however that there is no performance threshold
  or implementation optimisation that should exclude a given tool from entering
  the family of build systems. For an inefficient sorting algorithm is still a
  sorting algorithm, the same applies to build systems.


[^irony]: Ironically, this may explain why they are unloved[ALC]. They only get
  noticed when they are in the way, because it means that they do not live up
  to the expectations of their users.

[^Dataflow]: A Formal Definition of Data Flow Graph Models KRISHNAM KAVI, BILLP
  BUCKLES, ANDU NARAYANBHAT
  http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.589.9759

This comparison with sorting algorithms hints at something running deeper.
Build systems are tools defined by the similar problem they solve. We could
consider a build system any tool that implements a scheduling algorithm for a
set of tasks. Like sorting algorithms there are many variations depending on
special properties of the input. Unlike sorting algorithms however, build
system algorithms face _a lot_ of variations in the input formats,
expectations, and different implementations. <!-- It makes it difficult to
define what a build system is just from the problem it solves. ** Is that even
true ? ** -->

### Definition of build system

<!-- Build systems are software components that generate and maintain products
resulting from the execution of interdependent processes. -->

From all the aspects we forged the following definition.

> Build systems are software components that automate the generation of
> software products by scheduling the execution of a set of potentially
> interdependent processes.

By this definition, we limit ourselves to the software world. Car assembly
lines for example are not included, even though they do automate the production
of products. It is important because physical artifacts come with extra
constraints on their production. While, a newly generated file can overwrite
the previous one, it is not the case for cars.

Further on, the emphasis is put on software products, rather that on the
processes that produce them. These processes are a means to an end, and not the
subject of the build system. With this wording we exclude software systems
devoted to start and monitor services. `Systemd`, Erlang monitors or the
venerable `init` are out of scope because their focus is not on producing
something, but on running something, and keeping it running.

Nevertheless, the definition insists on tasks as the core issue to tackle.
These tasks have interdependencies that must be taken into account, and are the
sole way to produce the desired products. Task managers like `grunt` are
included in this definition because that is exactly what they do. But as we may
see in a future article, the outputs they produce consists more of effects on a
system and not software products _per se_. That makes them a bit apart amongst
build systems

Finally, all the subtlety and the complexity of writing _"good"_ build systems
resides in the way they schedule the execution of the processes. The most
important aspect of which is _incremental execution_. The technique consists in
_not_ running a task if it is not needed. Or conversely, running only tasks
needed to regenerate outdated products.

In future episodes, we will explore what makes a good schedule, how the choice
of schedules is constrained by the upfront information about the processes, as
well as the information that can be collected during and after their execution.

This design space is interspersed with existing implementations that come with
peculiar choices and specific solutions according to their main concern. The
variety of approaches to this single problem is captivating. And that they all
work only under a specific set of assumptions or requirements on their
execution environment is thrilling.

<!--

See [@mokhov_2018]


Input -> Process -> product.

meson >>= ninja
-->

