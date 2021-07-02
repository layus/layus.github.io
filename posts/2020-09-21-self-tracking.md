---
title: "Oblivious build systems: obviously incorrect?"
---


A big pain point with Make is that if you change a rule, the changes are not picked up by the next invocation. Nor by any following invocation.
While Make tracks your full build, it is oblivious of the Makefile itself.

To work around this limitation and restore [correctness](/tags/correctness.html) of `make` invocations, it is sufficient to make all the rules depend on the Makefile itself. If you have one, why not use your sledgehammer to kill the fly :-). The technique is inconvenient in two ways. First, while sufficient, it is not necessary. Any change to the Makefile will trigger a full rebuild. More often than not that's way beyond overkill. Secondly, it is quite tedious to do, and error prone. The dependency must be added _everywhere_. The other option is to remember to _clean_ the build when structural changes are made. And that remains an overkill.

That limitation is the reason why I started using Tup for my small projects, experiments and [grading](/tags/grading) homeworks. Tup was built around the idea that "\[t\]here must not be different ways of building the system that the user must remember in order to accommodate \[correctness or scalability issues in the build system implementation\]."[@shal:rules:2009, p. 7]
No need to `make clean`, no need to `make -B`, no need to `touch` anything. And no need to mentally review your changes to ensure none will slip under the radar of the build system.

<!--
So, make was bad, tup's better, article is over ?
-- Layus

No, because the issue of the scope cannot be solved. There are **always** things that fall out of scope of a build system.
Make ignores itself. Tup ignores updates to external files and excutables, etc. None of them track OS, cpu architecture, etc.
-->

So. Make is bad, Tup is good (_a praise to the beloved gods of Manichaeism_). The story could have ended there.

Yet recently I encountered another example of an oblivious build system: Hakyll.
This very blog is written using Hakyll. Hakyll is a static website generator (much like Jekyll) that takes the form of an Haskell DSL.
All the rules are written in an Haskell file, and the compiled output can be used to generate the static files.

```
# a typicall hakyll workflow
ghc site.hs -o site
./site build|watch|clean
```

You can spot the issue just by looking at the process. Because the generator is compiled, it has no way to know if the previous version of the site was compiled with the right rules.
This is the same situation as with Make.

## Self-tracking _<abbr title="French for 'to the rescue'">à la rescousse</abbr>_

The authors of "Build Systems _à la carte_" coined the adjective _self-tracking_ for build systems that are able to detect changes to build rules.
In the paper, the authors consider self-tracking as an optional feature of build systems: "Some build systems, for example Excel and Ninja, are capable of recomputing a task if either its dependencies change, _or_ the task itself changes."

The paper comes with a fully executable Haskell model of build systems where tasks are pure Haskell functions allowed to request (and hence depend upon) the output of other tasks.
This model needs tuning to describe self-tracking build systems. They present a setup where all the tasks are duplicated. For each task "foo" they introduce a task "foo-script" whose output is the rule that must be evaluated to perform the task.

From this experiment, we can see that the ability to depend (or track) a task definition itself requires the task to be expressible in terms of data, not as a process. And that that data requires some agreement on how it is evaluated.
In the paper, the authors state 

> The build systems that have precise self-tracking are all ones which use a _non-embedded
> domain-specific language_ to describe build tasks, that is, a task is described by a data
> structure (or syntax tree), and tasks can be compared for equality by comparing those data
> structures.

Because a non-embedded domain-specific language is textual data that must be interpreted.
Changes to that data before interpretation can be detected as a change to the task script, and the rebuilds propagated accordingly.

> Build systems that use a full programming language, for example Shake, are
> faced with the challenge of implementing equality on arbitrary tasks -and a task is just a
> function. For such build systems, the only safe approach is to assume (pessimistically) that
> any change to the build system potentially changes any build task –the classic example
> being build tasks depending on the makefile itself.

Ditto.

The relative importance given to Shake in "Build Systems _à la carte_" is better understood by explaining that Among the three authors Andrey Mokhov developed the model, in _Haskell_, Neil Mitchel is the original author and still the maintainer of Shake, and Simon Peyton Jones is one of the authors and maintainers of GHC, the most used _Haskell_ compiler.
Hadrian, the GHC build system, is also based on Shake and was developed by Andrey Mokhov.

## Can we live without self-tracking?

A correct build system should bring the system under consideration to a consistent state. When it is executed, all the outputs should be up-to-date with respect to their dependencies and their build rule.

An oblivious build system is therefore not correct. You cannot rely on it to bring the system to a consistent state.
This consistency is the second criterion for correctness as defined formally in "Build Systems _à la carte_", the first being quite irrelevant here.
And that formal definition also rules out oblivious build systems.

> The result is consistent with the `tasks`, that is, for all non-input keys `k` ∈ O,
> the result of recomputing the corresponding task matches the value stored in the
> result:
> ```haskell
>   getValue k result == compute task result.
> ```

Should a counter example be really needed, here is one. Just for the pleasure of using the well designed Haskell model from the paper. We build two similar sets of tasks, and use them in sequence. As the build system cannot detect changes in the task, it will not update the `"file"` output. Except for `busy` and other toy build system that rebuild unconditionally.

```haskell
tasks "file" = const $ return "content"
tasks' "file" = const $ return "updated content"

store = build tasks "file" emptyStore
-- store == Map.fromList [("file", "content")]
result = build tasks' "file" store
-- result == Map.fromList [("file", "content")]

-- getValue "file" result == "content"
-- compute (tasks "file") result == "updated content"
assert (getValue "file" result == compute (tasks' "file") result) -- fails
```

## Is it that bad?

Now that it is clear that self-tracking is required for correctness of build systems, and that we have effectively kicked Shake, Hakyll and the model from "Build Systems _à la carte_" out of the realm of correct build systems, I would like to take some time to reflect on the nature of correctness.

In the Haskell model of build systems, tasks are pure, and the environment on which they act is a key-value store that cannot possibly receive alterations outside of the build system.

In real-life, build system tasks depend on many impure things from their environment. Environment variables, compilers, libraries, OS, time, network, CPU, etc. From that perspective, correctness is a matter of personal taste. Should your build system ensure a consistent state after an update to external libraries ? And after an update to your installed compiler ?
Should it detect updates to the OS version? Or CPU firmware updates ?

There are different scopes in the things to track in the environment. It makes little sense to ensure correctness with respect to the CPU supplementary capabilities (MMX for example) when nothing is done to track the compiler version. The scope hierarchy derives from the frequency of changes in your environment.

An uncatched change in your compiler version has a much higher probability to invalidate the build than an update to your OS kernel whose API is usually more stable.
In this hierarchy, the probable impact and frequency of build system rules (or tasks) changes is quite high. At a time where build systems strive to achieve correctness with respect to larger and larger scopes, an oblivious build system seems to miss an obvious opportunity to ensure correct builds.





<!--
http://gittup.org/tup/build_system_rules_and_algorithms.pdf
https://stackoverflow.com/questions/4150830/depend-on-the-make-file-itself
https://stackoverflow.com/questions/3871444/making-all-rules-depend-on-the-makefile-itself
-->
