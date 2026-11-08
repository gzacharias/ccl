;;; Cross-compile setup for the CVM (the ccl-vm bytecode target).
;;; Parallel to tools/xdarwinarm64.lisp and tools/xdarwinx8664.lisp.
;;;
;;; Loads the darwincvm backend into a host that isn't the VM, then
;;;
;;;     (ccl:bc-compile-ccl :force t :output "ccl:ccl-bc;")
;;;
;;; compiles the CCL sources into a bytecode bundle for the VM.

(in-package "CCL")

(export 'bc-compile-ccl)

(defun load-cvm-backend (&optional force-compile)
  "Load the CVM backend, compiling its fasls first if they are missing or out of date."
  (with-compilation-unit ()
    (dolist (module *cvm-compiler-modules*)
      (update-modules module force-compile)
      (require-modules module))
    ;; Needs foreign-types, which every host has.
    (unless (backend-target-foreign-type-data (find-backend :darwincvm))
      (setup-cvm-ftd))
    (dolist (module *cvm-xload-modules*)
      (update-modules module force-compile)
      (require-modules module))))

(load-cvm-backend)

(defun bc-compile-ccl (&key force (output "ccl:ccl-bc;"))
  "Compile the CCL sources into a bytecode bundle for the VM, in OUTPUT."
  (let* ((*features* *features*)
         (*save-source-locations* NIL)
         (*cerror-on-constant-redefinition* t)
         (*package* (find-package :ccl))
         (*save-doc-strings* t)
         (*fasl-save-doc-strings* t)
         ;; So that gensyms dumped in the .bc files get the same names in every build.
         (*gensym-counter* 0)
         (*.fasl-pathname* (backend-target-fasl-pathname *cvm-backend*))
         (target (backend-name *cvm-backend*)))

    (when force
      (map nil #'delete-file (directory (make-pathname :name :wild
                                                       :type (pathname-type *.fasl-pathname*)
                                                       :directory '(:absolute :wild-inferiors)
                                                       :host "ccl")
                                        ;; works around a bug with .#xxx files
                                        :follow-links nil)))

    (with-global-optimization-settings ()
      (with-cross-compilation-target (target)
        (let ((*target-backend* *cvm-backend*))
          (cvm-compile-level-0)))
      (cross-compile-ccl target (not (null force)))
      ;; KLUDGE: cross-compiling doesn't compile the target's xload modules, rebuild-ccl does.
      ;; Compile them here too, although they're not needed, just so bundle made by here and
      ;; one made by rebuild-ccl in the VM have the same files, for easier sanity checks.
      (with-cross-compilation-target (target)
        (let ((*target-backend* *cvm-backend*))
          (target-compile-modules *cvm-xload-modules* target (not (null force))))))

    (when output
      (collect-all-fasls output))))


;; this is mostly for testing
(defun bc-compile (lambda)
  "Compile LAMBDA with the CVM backend, from a ccl running on a real machine."
  (let ((target (backend-name *cvm-backend*)))
    (with-cross-compilation-target (target)
      (let ((*target-backend* *cvm-backend*))
        (compile-named-function lambda :target target)))))

(defun test-fn (lambda-expr &key (print t) &aux (sym (make-symbol "NEW-TEST-FN")))
  (when (eq (car lambda-expr) 'defun)
    (setq lambda-expr (cons 'lambda (cddr lambda-expr))))
  (assert (lambda-expression-p lambda-expr))
  (let* ((fn (with-cross-compilation-target (:darwincvm);;set up packages
               (compile-named-function lambda-expr
                                      :name sym
                                      :target :darwincvm
                                      :keep-lambda *save-definitions*
                                      :keep-symbols *save-local-symbols*)))
         (bclambda (lfun-bclambda fn)))
    (if print
      (pprint bclambda)
      bclambda)))
  
