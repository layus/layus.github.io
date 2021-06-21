---
title: Reproducibility killed the Frankenbuild
author: Guillaume Maudoux
tags: ["build systems", "nix"]
---

They have been around for far too long. It's about time to tackle those pesky Frankenbuilds. Coined in 2016 by Esfahani et al. the term describes "builds where outputs from different build jobs can combine in inconsistent ways due to cache re-use".

But let's start with an example scenario for the problem. Imagine a build systems that produces an executable and the results of some test-suite on that executable. Both get added to the cache, but the cache entry gets later deleted to recover space or for any other reason. On the next build, the cache miss forces a re-build of the executable, which allows to refresh the cache. While it is exactly how the cache is supposed to work, it is left in an inconsistent state, where the test logs correspond to an executable that does not exist anymore.
The same situation appears when you compress an artefact, when you generate logs of execution, when you count the number of tests failures from a test log, and more generally when you cache some output based on a previous output and evict only the later from the cache.

As these examples highlight, a frankenbuild is an inconsistent build state where some build outputs are generated correctly but become incoherent with inputs generated separately (either later on, or in parallel). 
First a build output $\alpha$ needs some variability that escapes the vigilance of the build system. Then there needs to be some other build output $\beta$ that depends on $\alpha$ and exposes some of its variability.
In that situation, evicting $\alpha$ from the cache and regenerating it has a chance produce on output $\alpha'$ incompatible with $\beta$ in the sense that there is literally no way that $\beta$ could have been generated from $\alpha'$.
This violates the necessary condition for the build to be correct, as all the output are no more up-to-date with respect to their respective inputs.

The situation can happen in several ways, but only under some circumstances.
For it to happen we need two related but incompatible outputs to be generated. Unfortunate cache eviction and output regeneration can trigger it, but two builds running in parallel can race to fill the cache, and each get only one of their two outputs stored.Situations involving several caches also hide traps, as the primary cache may hold only some of the outputs while the fallback cache holds more of an unrelated generation of outputs, either due to eviction, speed of cache population or variation in cache population strategies.

For frankenbuild to exists outputs $\alpha$ needs to compete for the same cache entry as $\alpha'$ and $\beta$ the same as $\beta'$, should it have been built.
This means that the cache index of $\beta$ does not take a fingerprint of $\alpha$ into account. As is the case with build systems like Buck and Nix who computes cache keys based on the transitive closure of build instructions, and not the bits of intermediate results. This shows one weakness of these indexing schemes.
As for $\alpha$ and $\alpha'$, they have to compete for the same key, because the variability resides inside the build, 


## Mitigation techniques

Frankenbuilds are real threats to build theoretical correctness, but chiefly they could end-up in long hair-pulling sessions where before finding that the two involved inputs are incompatible. As would be the case when you debug an executable with similar but incompatible debug symbols. 

Quite surprisingly, the problem is not regarded as a severe threat to validity in the Nix community, which has high standards regarding build correctness.
That is because the Nix community caches are populated by a central build server who schedules all the builds. Two different builds never compete for the same cache buckets. As for cache eviction, no package has ever been evicted (yet) from the central package cache of nix packages. With these two aspects, the main cache of packages is always consistent.
With the advent of multiple caches, populated and evicted differently, the issue may gain a more pressing interest.

Microsoft CloudBuild also faced the issue, and worked around it by devising a protocol for builders that prevents racy builds to pollute the cache. Only the first builder of an output gets to write to the cache. The following racy builders have to discard their own artifacts and download the one already in the cache. This avoids overwriting output $\alpha$ by an $\alpha'$ version incompatible with the $\beta$ already in the cache.
As CloudBuild cache keys are not vulnerable to $\beta/\beta'$ collisions, this is sufficient to prevent Frankenbuilds.

In both cases, centralisation is used to guarantee one unique source of trust and central, valid cache.

## Provenance and the build paths

In the process, Esfahani et al. came with the concept or "provenance relation". It captures the idea that a build can only be known to be correct if all the outputs are known to have been built from the other outputs that are part of the same build.

## Fixes




