(in-package :ccl)

(defun load-cvm-target ()
  ;; have to compile stuff so require can find it.
  (compile-file "ccl:compiler;cvm;cvm-arch" :output-file "ccl:bin;cvm-arch" :load t)
  (compile-file "ccl:compiler;cvm;cvm-backend" :output-file "ccl:bin;cvm-backend")
  (compile-file "ccl:compiler;cvm;cvm2" :output-file "ccl:bin;cvm2" :load t)
  (load "ccl:lib;cvm-fcomp.lisp"))

(load-cvm-target)

(defun edit-cvm-target ()
  (ed "ct:cvm-target.lisp")
  (ed "ccl:lib;cvm-fcomp.lisp")
  (ed "ccl:compiler;cvm;cvm-arch.lisp")
  (ed "ccl:compiler:cvm;cvm-backend.lisp")
  (ed "ccl:compiler;cvm;cvm2.lisp"))
