# bc-compiler

This is a branch of Clozure CL which can compile CCL code into lisp-like bytecode for
loading into [ccl-vm](https://github.com/gzacharias/ccl-vm), a virtual CCL runtime that can
run inside any common lisp.  Instead of fasl files, it produces `.bc` files containing plain
text lisp expressions, which can be copied to any machine or checked into source control.

This branch is merged periodically with upstream Clozure CL `master`.  The source of the most recent merge
is stored as the `upstream-base` branch.

This branch has changes that implement the bc compiler, but is otherwise a fully functional CCL,
including supporting the IDE.


## Implementation strategy

The bc-compiler is implemented as a new backend to the compiler, stored in `compiler/CVM/`, and the `.bc` files are produced by a modified file compiler in `lib/cvm-fcomp.lisp`.

When running in the ccl-vm host, the CVM backend serves as the host backend.  When running in any other host, it is invoked as a cross-compiler as detailed below.

## Interaction with upstream

The `bc-compiler` is a branch of CCL containing the bytecode cross compiler and any general CCL source changes necessary to support it.  This is the main project branch.

Upstream changes come in by fast forwarding `upstream-base` to the current upstream `master` and merging into `bc-compiler`.

Changes to core CCL that stand on their own (bug fixes and improvements that seem generally advisable) are committed on `bc-compiler` like everything else, and are also offered upstream as pull requests.  Once upstream accepts one, the next merge from upstream brings it in again.

## Building and cross-compiling

**1. Get the bc-compiler**

<pre>
# check out the sources
git clone https://github.com/gzacharias/ccl.git
cd ccl
git checkout bc-compiler

# Get the <a href="https://github.com/Clozure/ccl/releases/tag/v1.14">ccl 1.14</a> binary assets for your system. For example for Linux/x86, do:
mytar=linuxx86
curl -L -O https://github.com/Clozure/ccl/releases/download/v1.14/$mytar.tar.gz
tar xf $mytar.tar.gz
alias ccl=./lx86cl64  # name of your kernel
</pre>

**2. Rebuild CCL from this branch.** The branch adds new files to the build, so load the updated `compile-ccl`
into the standard image when rebuilding for the very first time.  Two rebuilds are needed to complete the bootstrap.
```
ccl -n
(let ((*warn-if-redefine-kernel* nil))
  (load "ccl:lib;compile-ccl.lisp"))
;; First rebuild, expect warnings since it doesn't know about our changes
(rebuild-ccl :clean t)
(quit)

ccl -n
;; Second (and any subsequent) rebuild should be warning-free
(rebuild-ccl :clean t)
(quit)
```
**3. Compile CCL to bytecode.**

```
ccl -n
(load "ccl:tools;xdarwincvm.lisp")
(ccl:bc-compile-ccl :force t :output "ccl:ccl-bc;")
```

This puts the .bc files in the directory specified by `:output` (default `"ccl:ccl-bc;"`).


**4. Run it in ccl-vm.** See [ccl-vm README](https://github.com/gzacharias/ccl-vm#readme) for details.

## Status

* bc-compiler is a fully functional ccl fork that passes all ccl-tests on MacOS arm64 and x8664, and Linux arm64 and x8664 (there is no reason to believe it wouldn't pass on other hosts, I just don't have the means to test them)
* bc-compiler produces byte-identical bc bundles running in any of the 4 tested hosts
* Only the macOS target (`darwincvm` target) is supported ([Issue #9](https://github.com/gzacharias/ccl/issues/9))
* bc-compiler running in ccl-vm can compile itself (see [ccl-vm README](https://github.com/gzacharias/ccl-vm#readme))
* bc-compiler running in ccl-vm cannot compile stock ccl at this time ([Issue #2](https://github.com/gzacharias/ccl/issues/2))
* bc-compiler running in ccl-vm can cross-compile itself for the `darwinx8664` target (see [ccl-vm README](https://github.com/gzacharias/ccl-vm#readme))



## License

Apache License 2.0, like the rest of Clozure CL. See [LICENSE](LICENSE).

---

*What follows is the original README of Clozure CL.*

# Clozure CL

This is the source code for Clozure CL.

Because CCL is written in itself, you need an already-working version
of CCL to compile it.

See https://github.com/Clozure/ccl/releases/latest for instructions
on how to get a copy of CCL for your system.

To report a bug or request an enhancement, please make an issue at
https://github.com/Clozure/ccl/issues.

If you have questions or run into problems, send mail to
ccl-devel@clozure.com (see https://lists.clozure.com for instructions
on how to subscribe), ask on #ccl on libera.chat, or create an
issue here, especially if you think you have found a bug.
