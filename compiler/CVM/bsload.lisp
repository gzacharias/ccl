(in-package :ccl)

(defun bsload ()
  ;; have to compile stuff so require can find it.
  (compile-file "ccl:compiler;cvm;cvm-arch" :output-file "ccl:bin;cvm-arch" :load t)
  (compile-file "ccl:compiler;cvm;cvm-backend" :output-file "ccl:bin;cvm-backend")
  (compile-file "ccl:compiler;cvm;cvm2" :output-file "ccl:bin;cvm2" :load t)
  (load "ccl:lib;cvm-fcomp.lisp"))

(bsload)

(defun edit-bs ()
  (ed "ct:bsload.lisp")
  (ed "ccl:lib;cvm-fcomp.lisp")
  (ed "ccl:compiler;cvm;cvm-arch.lisp")
  (ed "ccl:compiler:cvm;cvm-backend.lisp")
  (ed "ccl:compiler;cvm;cvm2.lisp"))
