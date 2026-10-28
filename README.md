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

Unlike other compiler backends, this backend is always loaded and available regardless of the host it's running on.  When running in the ccl-vm host, it serves as the host backend.  When running on any other host, it is invoked as a cross-compiler via `ccl:bc-compile-ccl`

## Interaction with upstream

The `bc-compiler` is a branch of CCL containing the bytecode cross compiler and any general CCL source changes necessary to support it.  This is the main project branch.

Upstream changes come in by fast forwarding `upstream-base` to the current upstream `master` and merging into `bc-compiler`.

Changes to core CCL that stand on their own (bug fixes and improvements that seem generally advisable) are committed on `bc-compiler` like everything else, and are also offered upstream as pull requests.  Once upstream accepts one, the next merge from upstream brings it in again.

## Building and cross-compiling

Note: So far bc-compiler has only been tested with CCL running on an intel mac, as that's all I have.

**1. Get the bc-compiler**
```
git clone https://github.com/gzacharias/ccl.git
cd ccl
git checkout bc-compiler
# download ccl binary assets for the base version, which is ccl 1.14
curl -L -O https://github.com/Clozure/ccl/releases/download/v1.14/darwinx86.tar.gz
tar -xzf darwinx86.tar.gz
rm darwinx86.tar.gz
```

**2. Rebuild CCL from this branch.** The branch adds new files to the build, so load the updated `compile-ccl`
into the standard image when rebuilding for the very first time.  Two rebuilds are needed to complete the bootstrap.
```
./dx86cl64 -n
(let ((*warn-if-redefine-kernel* nil))
  (load "ccl:lib;compile-ccl.lisp"))
;; First rebuild, expect warnings since it doesn't know about our changes,
;; and rebuild kernel in case changed from release
(rebuild-ccl :clean t :kernel t)
(quit)

./dx86cl64 -n
;; Second (and any subsequent) rebuild should be warning-free
(rebuild-ccl :clean t)
(quit)
```
**3. Compile CCL to bytecode.**

```
(ccl:bc-compile-ccl :force t :output "ccl:ccl-bc;")
```

This puts the .bc files in the directory specified by `:output` (default `"ccl:ccl-bc;"`).  (Copies of the .bc files are also left in the sources, but don't rely on that, it's a bug.)


**4. Run it in ccl-vm.** See [ccl-vm README](https://github.com/gzacharias/ccl-vm#readme) for details.

## Status

* bc-compiler running in ccl-vm can cross-compile a native CCL.  So far this has been done for one target, darwinx8664 (Intel macOS): ccl-vm hosted in sbcl compiles ccl's sources and writes a bootstrapping image that the native kernel boots.
* Only the macOS target is supported (the target is called `:darwincvm`), because of issues with FFI constants.
* In theory the host running `ccl:bc-compile-ccl` shouldn't matter, but it has only been tested on an intel mac.

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
