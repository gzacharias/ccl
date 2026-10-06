# bc-compiler

This is a branch of Clozure CL which can compile CCL code into lisp-like bytecode for loading into [ccl-vm](https://github.com/gzacharias/ccl-vm), a virtual ccl runtime that can run inside any common lisp.  Instead of fasl files, it produces `.bc` files, which are plain text lisp expressions, and as such can be copied to any machine or checked into source control.

This branch is currently based on upstream Clozure CL `master` as of 2025-04-14 (commit `da7138ef`, based on version 1.13) and has not been merged with upstream since.

## Implementation strategy

The bc-compiler is implemented as a new backend to the compiler, stored in `compiler/CVM/`, and the `.bc` files are produced by the file compiler in `lib/cvm-fcomp.lisp`.

Unlike other compiler backends, this backend is always loaded and available regardless of the host it's running on.  When running in the ccl-vm host, it serves as the host backend.  When running on any other host, it is invoked as a cross-compiler via `ccl:bc-compile-ccl`

## What is in the branches

The `bc-compiler` branch contains the bytecode cross compiler.  This is the main project branch.

`bc-compiler` is descended from the `base-changes` branch, which contains changes to core CCL that stand on their own: bug fixes and improvements that seem generally advisable.  They are meant to be proposed for upstream CCL at some point.

`base-changes` is descended from `master`, which is an unmodified mirror of upstream master pinned at a particular commit.

To maintain the linear relationship between the three branches, follow any updates to the lower levels with rebasing of the higher levels.

## Building and cross-compiling

Note: bc-compiler has only been tested with ccl running on an intel mac.

**1. Get the bc-compiler**
```
git clone https://github.com/gzacharias/ccl.git
cd ccl
git checkout bc-compiler
# download ccl binary assets for the base version which is ccl 1.13
curl -L -O https://github.com/Clozure/ccl/releases/download/v1.13/darwinx86.tar.gz
tar -xzf darwinx86.tar.gz
rm darwinx86.tar.gz
```

**2. Rebuild CCL from this branch.** The branch adds new files to the build, so load the updated `compile-ccl`
into the standard image when rebuilding for the very first time.  Two rebuilds are needed to complete the bootstrap.
```
./dx86cl64 -n
(let ((*warn-if-redefine-kernel* nil))
  (load "ccl:lib;compile-ccl.lisp"))
;; First rebuild, expect warnings since it doesn't know about our changes
(rebuild-ccl :clean t)
(quit)

./dx86cl64 -n
;; Second (and any subsequent) rebuild should be warning-free
(rebuild-ccl :clean t)
(quit)
```

This branch has changes that implement `ccl:bc-compile-ccl`, but is otherwise a fully functional ccl, including supporting the IDE.

**3. Compile CCL to bytecode.**

```
(ccl:bc-compile-ccl :force t :output "ccl:ccl-bc;")
```

This puts the .bc files in the directory specified by `:output`(default `"ccl:ccl-bc;"`).  (Copies of the .bc files are also left in the sources, but don't rely on that, it's a bug.)


**4. Run it in ccl-vm.** See [ccl-vm README](https://github.com/gzacharias/ccl-vm#readme) for details.

## Status

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
