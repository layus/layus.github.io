---
title: How to Digest Nix Hashes ?
author: layus
tags: [nix]
published: 2020-09-01
---

They say there are two hard things in computing: cache invalidation and naming things. Alas, build systems fight against these two at the same time. Because caching build results is a central feature of most build systems, they are immediately concerned by cache invalidation issues. For naming things, it is less obvious. I used to understand the naming problem as related to concepts and source code variables. Finding the right name for a class, a function or a variable can be a real headache.

But it can also be difficult to generate a meaningful name for values manipulated by programs. Nix and a range of build systems have to forge unique and deterministic names for intermediate build results. These names are used as cache keys. In the case of Nix, the names are first class citizens as they are visible in the public store, in the form of store paths.

Did you ever wonder what's in your Nix hashes? Or how they are computed? Two different projects led me to further investigate these questions. Implementing [HNix] store and shepherding the "Content addressed paths" [RFC #62]. In both cases, understanding how path names (and the other hashes) are generated took some time.
Here is what I learned.

[HNix]: https://github.com/haskell-Nix
[RFC #62]: https://github.com/NixOS/rfcs/pull/62

Store path hashes
-----------------

The most visible digests in Nix appears in store path names. Lets take for example a pinned version of `hello`.

```nix
let
  nixpkgs = import (builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/c59ea8b8a0e7f927e7291c14ea6cd1bd3a16ff38.tar.gz";
    sha256 = "1ak7jqx94fjhc68xh1lh35kh3w3ndbadprrb762qgvcfb8351x8v";
  }) {};
in
  nixpkgs.hello # => /nix/store/ab1pfk338f6gzpglsirxhvji4g9w558i-hello-2.10
```

Nix composes the hash part of the path by compressing to 32 base32 characters (160 bits or 20 bytes) a sha256 digest. Base32 encoding is unique to Nix. It uses digits and lower-case letters (except [EOUT]) for a total of 32 characters valid in a file name. The result is more dense than base16 while  avoiding the strange characters of base64 (`+`, `/`, `=`) amongst which `/` would create a lot of trouble in file names.

[EOUT]: https://discourse.nixos.org/t/no-hashes-starting-with-e-t-o-or-u-in-nix-store/4906/1

For example `/nix/store/ab1pfk338f6gzpglsirxhvji4g9w558i-hello-2.10` contains <code><b>ab1pfk338f6g</b>zpglsirxhvji4g9w558i</code> which is the compression on 20 bytes of <code>0fqqilza6ifk0arlay18<b>ab1pfk338f6g</b>zrpcb56pnaw245h8gv9r</code>. Basically folding excess bits with xor. Notice how some characters are shared, as the input is so small that some of them are passed as-is.

```
  ab1pfk338f6gzrpcb56pnaw245h8gv9r
^             0fqqilza6ifk0arlay18
  --------------------------------
= ab1pfk338f6gzpglsirxhvji4g9w558i
```

The full (uncompressed) digest comes from hashing the string `output:out:sha256:5d4447675168bb44442f0d225ab8b50b7a67544f0ba2104dbf74926ff4df1d1e:/nix/store:hello-2.10`. This string is a fingerprint of the important parts of that derivation.
If any part changes, the hash will be different, and it will produce a different output path.

We can see four parts:

1. `output:out` is the type of the fingerprint. Here we fingerprint something used for an output path. The output named `out` in this case.
  Nix uses various types, and each expects different things in the remainder of the string.
2. `sha256:5d4447675168bb44442f0d225ab8b50b7a67544f0ba2104dbf74926ff4df1d1e` is the hash of the derivation building hello. As a hash it encompasses many things, and we will explore that further below.
3. `/nix/store` is the store prefix.
4. `hello-2.10` is the name of the derivation.

Hashing the derivation is a tricky part. Nix store a lot of information about derivations. To read it, we can use `nix show-derivation` on our `hello` package.


```json
{
  "/nix/store/4pmrswlhqyclwpv12l1h7mr9qkfhpd1c-hello-2.10.drv": {
    "outputs": {
      "out": { "path": "/nix/store/ab1pfk338f6gzpglsirxhvji4g9w558i-hello-2.10" }
    },
    "inputSrcs": [ "/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh" ],
    "inputDrvs": {
      "/nix/store/fkz4j4zj7xaf1z1g0i29987dvvc3xxbv-hello-2.10.tar.gz.drv": [ "out" ],
      "/nix/store/fsqdw7hjs2qdcy8qgcv5hnrajsr77xhc-bash-4.4-p23.drv": [ "out" ],
      "/nix/store/q0kiricfc0gkwm1vy3j0svcq5jib4v1g-stdenv-linux.drv": [ "out" ]
    },
    "platform": "x86_64-linux",
    "builder": "/nix/store/6737cq9nvp4k5r70qcgf61004r0l2g3v-bash-4.4-p23/bin/bash",
    "args": [ "-e", "/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh" ],
    "env": {
      "name": "hello-2.10",
      "out": "/nix/store/ab1pfk338f6gzpglsirxhvji4g9w558i-hello-2.10",
      "src": "/nix/store/3x7dwzq014bblazs7kq20p9hyzz0qh8g-hello-2.10.tar.gz",
      "stdenv": "/nix/store/50780gywsyjad8nxrf79q6qx7y7mqgal-stdenv-linux",
      // [elided for brevity]
    }
  }
}
```

`show-derivation` pretty prints and reformats the content of the .drv file. The exact content of `/nix/store/4pmrswlhqyclwpv12l1h7mr9qkfhpd1c-hello-2.10.drv` is a nested structure starting with `"Derive("` and that's why we will call it the _derive_ string. It is formatted as an [ATerm] from the Stratego language. Curious readers will find more about this format in the related [nix pill].

[Aterm]: http://releases.strategoxt.org/strategoxt-manual/unstable/manual/chunk-chapter/stratego-terms.html#id3314115 
[nix pill]:  http://lethalman.blogspot.com/2014/07/nix-pill-6-our-first-derivation.html

```
$ cat /nix/store/4pmrswlhqyclwpv12l1h7mr9qkfhpd1c-hello-2.10.drv
Derive([("out","/nix/store/ab1pfk338f6gzpglsirxhvji4g9w558i-hello-2.10","","")],[("/nix/store/fkz4j4zj7xaf1z1g0i29987dvvc3xxbv-hello-2.10.tar.gz.drv",["out"]),("/nix/store/fsqdw7hjs2qdcy8qgcv5hnrajsr77xhc-bash-4.4-p23.drv",["out"]),("/nix/store/q0kiricfc0gkwm1vy3j0svcq5jib4v1g-stdenv-linux.drv",["out"])],["/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh"],"x86_64-linux","/nix/store/6737cq9nvp4k5r70qcgf61004r0l2g3v-bash-4.4-p23/bin/bash",["-e","/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh"],[("buildInputs",""),("builder","/nix/store/6737cq9nvp4k5r70qcgf61004r0l2g3v-bash-4.4-p23/bin/bash"),("configureFlags",""),("depsBuildBuild",""),("depsBuildBuildPropagated",""),("depsBuildTarget",""),("depsBuildTargetPropagated",""),("depsHostHost",""),("depsHostHostPropagated",""),("depsTargetTarget",""),("depsTargetTargetPropagated",""),("doCheck","1"),("doInstallCheck",""),("name","hello-2.10"),("nativeBuildInputs",""),("out","/nix/store/ab1pfk338f6gzpglsirxhvji4g9w558i-hello-2.10"),("outputs","out"),("patches",""),("pname","hello"),("propagatedBuildInputs",""),("propagatedNativeBuildInputs",""),("src","/nix/store/3x7dwzq014bblazs7kq20p9hyzz0qh8g-hello-2.10.tar.gz"),("stdenv","/nix/store/50780gywsyjad8nxrf79q6qx7y7mqgal-stdenv-linux"),("strictDeps",""),("system","x86_64-linux"),("version","2.10")])
```

However, hashing this derive string directly does not yield the expected hash found above. (Do you recall the `sha256:5d4447675168bb44442f0d225ab8b50b7a67544f0ba2104dbf74926ff4df1d1e` ?)

```
$ nix-hash --flat --type sha256 /nix/store/4pmrswlhqyclwpv12l1h7mr9qkfhpd1c-hello-2.10.drv
40289ac3cc7d8896122c9a93ce580fb657aa29af6cf0a2bc4a30b3c53172ccf6
```

To understand where this hash comes from, it helps to understand other types of store objects.
Let's make a small detour to simpler paths.


Text files
----------

Raw text files whose content is know by Nix without running any builder are named by a comparatively simpler scheme. Their name is a digest over both the content and the name of the path.

```
$ nix-instantiate --eval --expr 'builtins.toFile "file-name" "some content"'
/nix/store/gn48qr23kimj8iyh50jvffjx7335k9fz-file-name
└── gn48qr23kimj8iyh50jvffjx7335k9fz
    └── 0cl4lvq60bp9il749fyngn48qr23kimj8xalivaxf55lnp41s7h9
        └── "text:sha256:290f493c44f5d63d06b374d0a5abd292fae38b92cab2fae5efefe1b0e9347f56:/nix/store:file-name"
            └── 290f493c44f5d63d06b374d0a5abd292fae38b92cab2fae5efefe1b0e9347f56
                └── "some content"
```

The text format is also used by .drv files. But .drv files can depend on other .drv files. All the dependencies appear between the `text:` and `:sha256` part of the path description.

```
/nix/store/4pmrswlhqyclwpv12l1h7mr9qkfhpd1c-hello-2.10.drv
└── 4pmrswlhqyclwpv12l1h7mr9qkfhpd1c
    └── 1c3ws0r5wm3ydx1zijcf4pmrswlhqyclxvqxqlqmv0spmfgg6zd2
        └── "text:[... dependant .drv's  ...]:sha256:40289ac3cc7d8896122c9a93ce580fb657aa29af6cf0a2bc4a30b3c53172ccf6:/nix/store:hello-2.10.drv"
            └── 40289ac3cc7d8896122c9a93ce580fb657aa29af6cf0a2bc4a30b3c53172ccf6
                └── "Derive([("out","... [content of /nix/store/4pmrswlhqyclwpv12l1h7mr9qkfhpd1c-hello-2.10.drv] ..."
```

All the different store path description strings are listed in [store-api.cc]. We have already seen 'text' right now and 'output' before. The third type is 'source', for some well-behaved, content addressed paths.

[store-api.cc]: https://github.com/NixOS/nix/blob/691a1bd7179bcf88f2638c1b8574c81f61e20786/src/libstore/store-api.cc#L63-L140

Hashing modulo
--------------

Back to our initial `pkgs.hello` store path. We were stuck at understanding the hash used there.

```
/nix/store/ab1pfk338f6gzpglsirxhvji4g9w558i-hello-2.10
└── ab1pfk338f6gzpglsirxhvji4g9w558i
    └── 0fqqilza6ifk0arlay18ab1pfk338f6gzrpcb56pnaw245h8gv9r
        └── "output:out:sha256:5d4447675168bb44442f0d225ab8b50b7a67544f0ba2104dbf74926ff4df1d1e:/nix/store:hello-2.10"
            └── 5d4447675168bb44442f0d225ab8b50b7a67544f0ba2104dbf74926ff4df1d1e
                └── ???
```

Ideally, this would be the hash of the 'derive' string from the .drv. This is not the case because it would be impossible and undesirable.
The impossibility comes from the fact that the output paths of a derivation are part of its 'derive' string.
The loop needs to be broken somewhere. That is why the output paths are replaced with empty strings before hashing the 'derive' string used in output paths.

The other aspect comes from fixed-output paths. While the recipe to build them may vary (and hence their derive string) we would like to avoid propagating such changes to other derivations outputs.
As fixed-output derivations can happen anywhere in the dependency tree, the process of replacing the hash of fixed-output derivations needs to be recursive. This is performed by `hashDerivationModulo()` whose name hints that the hashing is made modulo the equivalence of recipes for the same fixed-output paths.

It means that instead of the former `nix show-derivation` result, hashDerivationModulo ends up hashing a modified derive string.

```json
{
  "/nix/store/4pmrswlhqyclwpv12l1h7mr9qkfhpd1c-hello-2.10.drv +mased +modulo": {
    "outputs": {
      "out": { "path": "" } // masked
    },
    "inputSrcs": [ "/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh" ],
    "inputDrvs": {
      "103f297b7051255f2b7c1cd9838ee978d6ba392fb6ae2a6112d5816279c4ed14": [ "out" ], 
      // hash modulo fixed-ouput derivations of /nix/store/fsqdw7hjs2qdcy8qgcv5hnrajsr77xhc-bash-4.4-p23.drv
      "26f653058a4d742a815b4d3a3c0721bca16200ffc48c22d62b3eb54164560856": [ "out" ],
      // fixed hash of fixed-ouptut derivation /nix/store/3x7dwzq014bblazs7kq20p9hyzz0qh8g-hello-2.10.tar.gz
      // hash of the string 'fixed:out:sha256:31e066137a962676e89f69d1b65382de95a7ef7d914b8cb956f41ea72e0f516b:/nix/store/3x7dwzq014bblazs7kq20p9hyzz0qh8g-hello-2.10.tar.gz'
      "a9365c39d2b7a2a8f2340da6e9814ca605f8dcefe4b49f5c44db7d9ed3bb031f": [ "out" ]
      // hash modulo fixed-output derivations of /nix/store/q0kiricfc0gkwm1vy3j0svcq5jib4v1g-stdenv-linux.drv
    },
    "platform": "x86_64-linux",
    "builder": "/nix/store/6737cq9nvp4k5r70qcgf61004r0l2g3v-bash-4.4-p23/bin/bash",
    "args": [ "-e", "/nix/store/9krlzvny65gdc8s7kpb6lkx8cd02c25b-default-builder.sh" ],
    "env": {
      "name": "hello-2.10",
      "out": "", // masked
      "src": "/nix/store/3x7dwzq014bblazs7kq20p9hyzz0qh8g-hello-2.10.tar.gz",
      "stdenv": "/nix/store/50780gywsyjad8nxrf79q6qx7y7mqgal-stdenv-linux",
      // [still elided for brevity]
    }
  }
}
```

Recap
-----

As we have seen, hashing in Nix is based on several concepts.

Description strings
:   starting with a type, and separated with colons. Highly recognisable. They describe uniquely a ressource. In practice, we never encounter them, as they are always hashed with sha256.

Hash compression
:   The hash appearing in path names is a folded version of full digests. Nix compresses the hash to 32 base32 characters.

Maksing
:   _Derive_ strings contain the output paths of the derivation. As these output paths are generated based on a digest of the derivation itself, we have to break the loop. Masking is the process of removing output paths from a _derive_ string before computing it's hash.

Hashing modulo \[other derivations\]
:   Digests of derivations form a tree. Any change to a dependency's _derive_ string will propagate to all the dependant .drv paths and output paths. But it makes little sense to propagate changes in the recipe (the _derive_ string) of a fixed output path. By definition, they will produce the same output regardless of their recipe. Nix computes output paths hashes on a tree of digests where fixed-output _derive_ strings are replaced by the fixed-output hash.


Practical issues
----------------

So much for technical considerations. What is this useful for?
The way hashes are computed constrains how they can be computed and generated.

In an HNix [discussion](https://github.com/haskell-nix/hnix-store/pull/59#discussion_r429582370-permalink), I discovered that the Nix daemon has two API calls to build derivations.

`opBuildPaths` is the most obvious one. It takes a list of pre-uploaded .drv files and triggers the build. Because a .drv depends on other .drv files, the full closure needs to be uploaded to the store upfront.
In our example, that closure represents 280 .drv files to upload before starting the build. And `hello` is a relatively small package.
This can slow down build times in distributed remote building situation where the builder responsible for a package may not be the one that built its dependencies. The machine will have to download all the .drv files when all it really needs is the `hello` .drv file and the build inputs paths.

That is why `opBuildDerivation` was implemented. It takes all the information from a derivation and builds it. The input paths need to be uploaded beforehand, but nothing more.
This nice feature come with a downside. As we have seen, the ouptut path of a derivation is computed with hashDerivationModulo, which requires the full closure of derivations to substitute the hashe of fixed-output ones.
Without the closure, it is impossible for the builder to check the validity of an output path. That is why this API call is a privileged operation.

Allowing unprivileged builds without the .drv closure is not an easy feature. As Eelco Dolstra states in the [commit introducing `opBuildDerivation`](1511aa9f488ba0762c2da0bf8ab61b5fde47305d), it would require changing the hashing scheme. And finding the right balance is complicated. Using a hash "without modulo" means that changing _how_ we build fixed-output derivations will propagate to all the package.
Not hashing inputs makes it possible to obtain the same output path name for all the derivations that change only in their inputs. For example, updating gcc would not change our `hello` output path. You could get different things under the same name. Not an option.

That sentence from Eelco Dolstra feels a bit like Fermat's last theorem. While it seems to imply that there exists other hashing schemes, nothing is said about these schemes.
Years later (the commit dates from 2015), the solution comes from a different angle. The long discussed, argued (and ultimately postponed) feature of a content-addressed store could bring builds without the .drv closure. Much like the proof of Fermat's last theorem, the implementation of the content-addressed store comes long after the problem is sketched but, were it used only to solve that specific problem, would also feels like bringing an elephant to kill the mouse.

### Content-addressed paths

In the content-addressed store, output path names are not derived from their .drv, but from their content. Orthogonal changes to .drv files are not reflected in the name, and do not propagate. The name changes only if the content changes.
The major downside is that output path names cannot be known before their content is made available. That's why we need two names. On for the output path we want to build, and one for the actual content-addressed result.

In such a setup, there is no need anymore for `hashDerivationModulo`. I tend to see `hashDerivationModulo` as a hack, a workaround to limit useless rebuilds as much as possible without having to implement the content-addressed store in its full complexity. That hack served us well over the years, and content-addressed stores are not yet implemented. 

<!-- TODO: forward reference on intesion vs extension -->

Closing thoughts
----------------

There is already a lot in this article, but we did not cover everything. There are several ways to upload a content-addressed path to the store, and content-addressed paths can also depend on other content-addressed paths.
There are other funny corner cases here and there. But overall, this sketches the idea behind Nix store paths generation, and gives an idea of how derivations and store paths interact.

I had to leave aside the detailed explanation of content-addressed stores, but this is perhaps not for long...

This blog is still lacking a proper way to leave comments, but I would be more than happy to receive remarks, comments, advices and praises by email, or by any other channel if you are willing to wait more.

Some more stuff (a.k.a. Annexes)
--------------------------------

Things that did not fit elsewhere.

A/ Python based hash compression.

```python
""" XORing directly in base32, thanks to python """

h = "0fqqilza6ifk0arlay18ab1pfk338f6gzrpcb56pnaw245h8gv9r"
key = "0123456789abcdfghijklmnpqrsvwxyz" # no e,o,u,t
# len(key) == 32

def xor(a, b):
    return key[key.index(a) ^ key.index(b)]
# xor('0', 'a') == 'a'

h1, h2 = h[-32:], "{:0>32}".format(h[:-32])
res = "".join(xor(h1[i],h2[i]) for i in range(32))
print("  {}\n^ {}\n  ---------------------------------\n= {}".format(h1,h2,res))
# prints:
"""
  ab1pfk338f6gzrpcb56pnaw245h8gv9r
^ 0000000000000fqqilza6ifk0arlay18
  ---------------------------------
= ab1pfk338f6gzpglsirxhvji4g9w558i
"""
```

B/ Nix source code patch to trace digests being computed.

```diff
diff --git a/src/libutil/hash.cc b/src/libutil/hash.cc
index 4a94f0dfd..b06a08c79 100644
--- a/src/libutil/hash.cc
+++ b/src/libutil/hash.cc
@@ -316,6 +316,15 @@ Hash hashString(HashType ht, std::string_view s)
     start(ht, ctx);
     update(ht, ctx, (const unsigned char *) s.data(), s.length());
     finish(ht, ctx, hash.hash);
+    if (s.length() > 500 && s.data()[0] != 'D') {
+        warn("Hashing %d characters with '%s'", s.length(), ht);
+        warn("base32: %s", hash.to_string(Base32, true));
+    } else {
+        warn("Hashing '%s' with '%s'", s, ht);
+        warn("base16: %s", hash.to_string(Base16, true));
+        warn("base32: %s", hash.to_string(Base32, true));
+        warn("base64: %s", hash.to_string(Base64, true));
+    }
     return hash;
 }
 
@@ -380,6 +389,7 @@ Hash compressHash(const Hash & hash, unsigned int newSize)
     h.hashSize = newSize;
     for (unsigned int i = 0; i < hash.hashSize; ++i)
         h.hash[i % newSize] ^= hash.hash[i];
+    warn("Compressed '%s' to size %d: '%s'", hash.to_string(Base32, false), newSize, h.to_string(Base32, false));
     return h;
 }
```

C/ The full log of hashes generated for out `pkgs.hello` example with the above logging patch is available on [gist](https://gist.github.com/551fcccf4ddc1a851bc818ad2d21f8fb).
