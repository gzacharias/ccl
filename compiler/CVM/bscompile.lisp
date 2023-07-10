(in-package :ccl)

#|

;;; **** MAKE SURE WE'RE NOT REVERSING ORDER OF EVALUATION ANYWHEERE

;;; **** ARRANGE TO NOT SAVE DOC STRINGS, too slow to load.

%toplevel-function% in nfasload.lisp  gets called before cold load functions, and cold load functions
 are what initalizes the evaluator.

|#

  ;; (defparameter *cvm-compiler-modules* '(cvm-arch))
  ;; (defparameter *cvm-compiler-backend-modules* '(cvm-backend cvm2))


;; (test-vm)
;; TODO: figure out if can avoid compiling compiler/vinsn for example
;; l1-aprims when loaded defines a package and gives it nickname "OS".

(defparameter *level-1-modules-not-for-cmv*
  ;;;'(l1-lisp-threads l1-processes l1-sockets)
  '(l1-lisp-threads))

;; Well, we will want to be able to load these in the target.
(defparameter *compiler-modules-not-for-cvm* ())

(defparameter *aux-modules-not-for-cvm*
  '(edit-callers
    cover
    leaks
    core-files
    dominance
    sockets))

(defparameter *compiler-modules-not-for-cvm*
  '(reg))

(defparameter *code-modules-not-for-cvm*
  '(backtrace-lds))

;#+compile-level-0
(defun test-vm (&optional force)
  ;; Don't really understand the intended way of doing this.  Any attempt to
  ;; use a target ends up calling FIND-BACKEND, but there is no cvm backend until
  ;; these files are loaded, so just do it.
  (load "ccl:compiler;cvm;cvm-arch.lisp")
  (load "ccl:compiler;cvm;cvm-backend.lisp")

  ;; Stuff we've redefined.  Until build a new lisp
  (let ((*warn-if-redefine-kernel* nil))
    (load "ccl:lib;systems.lisp") ;; make sure we have the latest, avoid bootstrapping issuess.
    (load "ccl:lib;nfcomp.lisp")
    (load "ccl:lib;db-io.lisp")
    (load "ccl:lib;foreign-types.lisp")
    (load "ccl:lib;compile-ccl.lisp")
    (load  "ccl:compiler;nx1.lisp")
    (load "ccl:lib;macros.lisp"))

  ;; ok, comile level-0 and then look over the files.  The goal will be to be able to load
  ;; them in sbcl with the cvm-runtime package.
  (let* ((*build-time-optional-features* nil)
         (*features* *features*)
         (*save-source-locations* NIL #+no *ccl-save-source-locations*)
         (*cerror-on-constant-redefinition* t)
         (*warn-if-redefine-kernel* t)
         ;; Can't bind *.fasl-pathname* because that's used to load auxilliary macro files etc.
         (.fasl-pathname (backend-target-fasl-pathname *cvm-backend*))
         (*package* (find-package :ccl))
         (*save-doc-strings* t)
         (*fasl-save-doc-strings* t))
    ;; don't really need this now that putting everything in bsfasls;
    (when force
      (mapc #'delete-file (directory (merge-pathnames .fasl-pathname "ccl:level-0;**;*"))))
    (with-global-optimization-settings ()
      (dolist (dir '("ccl:level-0;" "ccl:level-0;CVM;"))
        (ensure-directories-exist "ccl:level-0;cvmfasls;")
        (let ((outpath (merge-pathnames "ccl:level-0;cvmfasls;" .fasl-pathname)))
          (loop for src in (sort (directory (merge-pathnames dir "*.lisp")) #'string< :key #'namestring)
            do (let* ((fasl (merge-pathnames outpath src)))
                 (when force (assert (not (probe-file fasl)))) ;; Check for duplicate filenames...
                 (when (or force
                           (not (probe-file fasl))
                           (> (file-write-date src)
                              (file-write-date fasl)))
                   (setq *nx-speed* (max 1 *nx-speed*))
                   (setq *nx-safety* (min 1 *nx-safety*))
                   ;; This sets up the target:: and os:: package nicknames and *target-ftd*
                   (with-cross-compilation-target (:darwincvm)
                     (compile-file src :target :darwincvm :features nil :output-file fasl :verbose t))))))))))
#+debug
(progn

  (macrolet ((expn (&rest names)
               `(progn
                  ,@(loop for name in names
                      collect `(defun ,name (&rest args) (cons ',name args))))))
    (expn $bs-package $bs-symbol $bs-cons-function $bs-init-function $bs-istruct-cell $bs-quote $bs-make-uvector $bs-init-uvector
          $bs-gvector $bs-uvector $bs-eval))

(defun ppfile (file)
  (with-open-file (stream (merge-pathnames file
                                           (merge-pathnames "ccl:level-0;cvmfasls;"
                                                            (backend-target-fasl-pathname *cvm-backend*))))
    (let ((*loader-table* nil))
      (declare (special *loader-table*))
      (loop for expr = (read stream nil stream) until (eq expr stream)
        as (op . args) = expr
        do (let ((*print-array* t))
             (pprint
              (cons op
                    (cond ((and (eq op 'setq) (eql (length args) 2))
                           (eval expr)
                           (setq *print-array* nil)
                           `(,(first args) ',(eval (second args))))
                          (t (mapcar 'eval args))))))))))
)

#+compile-all-except-level-0
(defun test-vm ( &optional force)
  ;; Don't really understand the intended way of doing this.  Any attempt to
  ;; use a target ends up calling FIND-BACKEND, but there is no backend until
  ;; these files are loaded, so just do it.
  (load "ccl:compiler;cvm;cvm-arch.lisp")
  (load "ccl:compiler;cvm;cvm-backend.lisp")
  ;; So at this point we have a compile-ccl loaded up, and systems loaded up.
  ;; So it has our changes, but it was compiled with target local system, so can't
  ;; really use #+target to 
  (let ((*warn-if-redefine-kernel* nil))
    ;; Stuff we've redefined.  Until build a new lisp
    (load "ccl:lib;systems.lisp") ;; make sure we have the latest, avoid bootstrapping issuess.
    (load "ccl:lib;db-io.lisp")
    (load "ccl:lib;foreign-types.lisp")
    (load "ccl:lib;compile-ccl.lisp")
    (load  "ccl:compiler;nx1.lisp")
    (load "ccl:lib;macros.lisp"))
  (let ((*level-1-modules*
         (set-difference *level-1-modules* *level-1-not-for-cvm*))
        (*compiler-modules*
         (set-difference *compiler-modules* *compiler-modules-not-for-cvm*))
        (*aux-modules*
         (set-difference *aux-modules* *aux-modules-not-for-cvm*))
        (*code-modules*
         (set-difference *code-modules* *code-modules-not-for-cvm*))
        (*compiler-modules*
         (set-difference *compiler-modules* *compiler-modules-not-for-cvm*)))
    ;; Ok, this is confusing.   RIGHT NOW, ON THIS HOST, we want to be running BSCOMPILE as pass2.
    ;;   But on the remote host, once we bootstrap, we want to be loading up a native compiler.
    ;;   Which doesn't exist yet.  But really will want to run whichever backend is  *target-backend*
    ;;  there, so have to be able to compile bscompile as well.
    ;; *** RIGHT NOW JUST TESTING HOW MUCH ARCH/BACKEND STUFF IS NEEDED TO COMPILE ALL OF CCL. Worry
    ;;  about runtime later.
    (cross-compile-ccl :darwincvm force)))

#|
   (target-compile-modules 'nxenv target force)
    (target-compile-modules *compiler-modules* target force)
    (target-compile-modules (target-compiler-modules arch) target force)
    (target-compile-modules (target-level-1-modules target) target force)
    (target-compile-modules (target-lib-modules target) target force)
    (target-compile-modules *sysdef-modules* target force)
    (target-compile-modules *aux-modules* target force)
    (target-compile-modules *code-modules* target force)
    (target-compile-modules (target-xdev-modules arch) target force)))
|#


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
         (bslambda (ev2-lfun-bslambda fn)))
    (if print
      (pprint bslambda)
      bslambda)))
  

  

(defparameter *real-backend* (find-backend :darwinx8664))
#+old
(defun test-fn (lambda-expr &key (print t) &aux (sym (make-symbol "TEST-FN")))
  (when (eq (car lambda-expr) 'defun)
    (setq lambda-expr (cons 'lambda (cddr lambda-expr))))
  (assert (lambda-expression-p lambda-expr))
  (let ((*host-backend* (copy-backend *host-backend*)))
    (declare (special *host-backend*))
    (setf (backend-p2-compile *host-backend*) 'ev2-compile)
    (handler-bind ((error (lambda (c)
                            (setq *host-backend* *real-backend*)
                            (error c))))
      (compile sym lambda-expr)))
  (let ((lambda (ev2-lfun-bslambda (symbol-function sym))))
    (if print
      (pprint lambda)
      lambda)))



(unadvise compile-named-function :name bscompile)
(unadvise x862-compile :name bscompile)
(unadvise find-module :name bscompile)
(unadvise compile-file :name bscompile)
(unadvise find-backend :name bscompile)
(unadvise fasl-dump-file :name bscompile)
(when (fboundp 'setup-xload-target-parameters)
  (unadvise setup-xload-target-parameters :name bscompile))
(when (fboundp 'xfasload)
  (unadvise xfasload :name bscompile))

;#+compile-for-vm
(progn
  ;; So all this needs to get vm versions, or be pre-built into the vm.
  ;"level-0/X86/X8664/x8664-bignum" "level-0/X86/x86-array" "level-0/X86/x86-clos" "level-0/X86/x86-def" "level-0/X86/x86-float"
  ;"level-0/X86/x86-hash" "level-0/X86/x86-io""level-0/X86/x86-misc""level-0/X86/x86-numbers""level-0/X86/x86-pred"
  ;"level-0/X86/x86-symbol""level-0/X86/x86-utils"

  ;"level-0/l0-aprims""level-0/l0-array""level-0/l0-bignum32" "level-0/l0-bignum64" "level-0/l0-cfm-support"
  ;"level-0/l0-complex""level-0/l0-def""level-0/l0-error""level-0/l0-float""level-0/l0-hash""level-0/l0-init""level-0/l0-int"
  ;"level-0/l0-io""level-0/l0-misc""level-0/l0-numbers""level-0/l0-pred""level-0/l0-symbol""level-0/l0-utils""level-0/nfasload"

  ;(bscompile-for-vm "ccl:level-0;l0-aprims.lisp")




  ;; (bscompile-for-vm "ccl:level-0;l0-aprims")
  (defun bscompile-for-vm (files &key (verbose t))
    ;(load "ev:bscompile.lisp")
    (require 'faslenv "ccl:xdump;faslenv")
    (unless (consp files) (setq files (list files)))
    (let* ((*features* (cons :cross-compiling *features*))
           (*.fasl-pathname* (backend-target-fasl-pathname *cvm-backend*)))
      (let* ((*build-time-optional-features* nil)
             (*save-source-locations* nil)
             (cd (current-directory))
             (*cerror-on-constant-redefinition* nil)
             (*warn-if-redefine-kernel* nil))
        (unwind-protect
            (with-global-optimization-settings ()
              (setf (current-directory) "ccl:")
              (loop for file in files
                as output-file = (merge-pathnames  file)
                ;; Compile file complains if it's not a fasl file.
                when (probe-file output-file) do (delete-file output-file)
                do (compile-file file
                                 :target :cvm
                                 :output-file output-file
                                 :verbose verbose)
                do (ed output-file)))
          (setf (current-directory) cd)))))

  (unadvise fasl-dump-file :name bscompile)
  (advise fasl-dump-file
          (progn
          (if (eq *fasl-target* :darwincvm)
            (destructuring-bind (gnames goffsets forms hash output-file) arglist
              (assert (null gnames))
              (assert (null goffsets))
              (format *TRACE-OUTPUT* "ev2-output to ~s" output-file)
              (ev2-output-compiled-file forms hash output-file))
            (:do-it)))
          :when :around :name bscompile)

  #+NO(defvar *use-bscompile-now* nil)

  (unadvise compile-named-function :name bscompile)
  #+NO(advise compile-named-function
          (let ((*use-bscompile-now* (and *compiling-file*
                                          ;; Only time we have a load-time-eval-token is when called form
                                          ;; fcomp-named-function or from nx1-load-time-value, exactly the
                                          ;; two cases we want to intercept.
                                          (not (eq (getf (cdr arglist) :load-time-eval-token 'no) 'no)))))

            (:do-it))
          :when :around :name bscompile)

  (unadvise x862-compile :name bscompile)
  #+NO(advise x862-compile
          (if *use-bscompile-now*
            (apply #'ev2-compile arglist)
            (:do-it))
          :when :around :name bscompile)


);;;#+compile-for-vm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Compiling files

(require 'faslenv "ccl:xdump;faslenv")

(defvar *ev2-fcomp-hash*)
(defvar *ev2-fcomp-eref*)
(defvar *ev2-bsquote*)

;; Really would be so much easier to intercept this in fcomp-form-1
(defun set-package-call-p (fn)
  (let ((bslambda (ev2-lfun-bslambda fn)))
    (destructuring-bind (name argspecs body nlocals) (cdr bslambda)
      (when (and (equal name '($bs-quote nil))
                 (every #'null argspecs)
                 (zerop nlocals)
                 (eql (length body) 3)
                 (eq (car body) '$bs-funcall)
                 (equal (cadr body) '($bs-quote ccl::set-package))
                 (eq (car (caddr body)) '$bs-quote))
        (cadr (caddr body))))))
        

(defun ev2-output-compiled-file (toplevel-forms hash output-file)
  ;;(assert (equalp (pathname-type output-file) (pathname-type (bscompile-fasl))))
  (with-open-file (outf output-file :direction :output :if-exists :supersede)
    (format outf "~&(cl:in-package :ccl-vm)~2%~
                    (SETQ *LOADER-TABLE* (MAKE-ARRAY ~d.))~2%"
            (hash-table-count hash))
    (let* ((*ev2-fcomp-hash* hash)
           (*ev2-fcomp-eref* -1)
           (*ev2-bsquote* nil))
      (loop for form in toplevel-forms as (op . args) = form
        as bs-opcode = (cond ((eq op $fasl-platform) nil)
                             ((eq op $fasl-src) nil #+NOT-YET '$fasl-record-source)
                             ((eq op $fasl-toplevel-location) 
                              (check-type (car args) (or source-note null))
                              nil #+NOT-YET '$fasl-toplevel-location nil)
                             ((eq op $fasl-lfuncall)
                              (check-type (car args) function)
                              (let ((pkg (set-package-call-p (car args))))
                                (if pkg
                                  (progn
                                    (setq args (list pkg))
                                    '$fasl-set-package)
                                  `$fasl-funcall)))
                             ((eq op $fasl-defun)
                              (check-type (car args) function)
                              '$fasl-defun)
                             ((eq op $fasl-defvar)
                              '$fasl-defvar)
                             ((eq op $fasl-defvar-init) '$fasl-defvar-init)
                             ((eq op $fasl-defparameter) '$fasl-defparameter)
                             ((eq op $fasl-defconstant) '$fasl-defconstant)
                             ;; args = fn, doc : "doc" is either a string or a list of the form :
                             ;; (doc-string-or-nil . (body-pos-or-nil . arglist-or-nil))
                             ((eq op $fasl-macro) '$fasl-defmacro)
                             (t (error "unsupported toplevel form ~s" form)))
        when bs-opcode
        ;; Ugh, wait, if all of them will be $BS-QUOTE, why do we bother???
        do (write (cons bs-opcode (mapcar #'(lambda (arg)
                                              (let ((*ev2-bsquote* t))
                                                (ev2-maker-form arg)))
                                          args))
                  :stream outf :pretty t :readably t :structure nil)
        and do (terpri outf)))))


(defun ev2-maker-form (obj)
  (let ((info (gethash obj *ev2-fcomp-hash*)))
    (cond ((fixnump info) `(aref *loader-table* ,info))
          ((eq info t)
           (let ((store-index (incf *ev2-fcomp-eref*)))
             (when (typep obj '(or (signed-byte 60) character boolean immediate))  ;; don't store immediates
               (setq store-index nil))
             (setf (gethash obj *ev2-fcomp-hash*) store-index)
             (ev2-maker-dispatch obj store-index)))
          ((null info)
           (ev2-maker-dispatch obj nil))
          (t
           (destructuring-bind (load-form scanned-p referenced-p compiled-initform) info
             (declare (ignore scanned-p))
             ;;(assert scanned-p)
             ;; If referenced-p is NIL means this FORM is referenced only once, so won't need to store it.
             ;; If referenced-p is T this means it got referenced more than once.  In this case, load-form info is at least T.
             (when referenced-p
               (check-type (gethash load-form *ev2-fcomp-hash*) (or fixnum (eql t))))
             (let ((maker (ev2-maker-form load-form)))
               (when referenced-p
                 (setf (gethash obj *ev2-fcomp-hash*) (gethash load-form *ev2-fcomp-hash*)))
               (if compiled-initform
                 `(prog1 ,maker ,(ev2-maker-form compiled-initform))
                 maker)))))))

(defun ev2-maker-dispatch (obj store-index)
  (cond ((typep obj '(or fixnum single-float character boolean)) (ev2-maybe-store obj store-index))
        ((eq obj (%unbound-marker)) (ev2-maybe-store '($fs-unbound-marker) store-index))
        ((eq obj (%slot-unbound-marker)) (ev2-maybe-store '($fs-slot-unbound-marker) store-index))
        ((eq obj (%illegal-marker)) (ev2-maybe-store '($fs-illegal-marker) store-index))
        ((typep obj 'number) (ev2-number-maker obj store-index))
        ((consp obj) (ev2-cons-maker obj store-index))
        ((symbolp obj) (ev2-symbol-maker obj store-index))
        ((typep obj 'function) (ev2-function-maker obj store-index))
        ((typep obj 'simple-base-string) (ev2-string-maker obj store-index))
        ((typep obj 'simple-vector) (ev2-simple-vector-maker obj store-index))
        ((typep obj '(simple-array * (*))) (ev2-ivector-maker obj store-index))
        ((typep obj 'simple-array) (ev2-array-maker obj store-index))
        ((typep obj 'package) (ev2-package-maker obj store-index))
        ((istructp obj) (ev2-istruct-maker obj store-index))
        ;; It wouldn't be hard to dump arbitrary gvectors/ivectors, but it's not needed.
        (t (error "invalid constant ref ~s" obj))))

(defun ev2-maybe-store (form store-index)
  (if store-index `(setf (aref *loader-table* ,store-index) ,form) form))

(defun ev2-string-maker (string store-index)
  (check-type string simple-string)
  (ev2-maybe-store (if *ev2-bsquote* `($fs-string ,string) string) store-index))

(defun ev2-number-maker (number store-index)
  (if *ev2-bsquote*
    (ev2-uvector-maker (etypecase number
                         (bignum :bignum)
                         (double-float :double-float)
                         (ratio :ratio)
                         (complex-double-float :complex-double-float)
                         (complex-single-float :complex-single-float)
                         (complex :complex))
                       number
                       store-index)
    (ev2-maybe-store number store-index)))

(defun ev2-package-maker (pkg store-index)
  (assert *ev2-bsquote*)
  (ev2-maybe-store `($fs-package ,(ev2-maker-form (package-name pkg))) store-index))

;; Could output symbols directly...  Except, maybe need to do the binding index thing?
;; Should at least output CL symbols directly
(defun ev2-symbol-maker (sym store-index)
  (let* ((inverse (fasl-setf-name-inverse-p sym)))
    (if inverse
      (progn
        (assert (null store-index)) ;; sym never got scanned so shouldn't have a store-index
        (ev2-maker-form inverse))
      (if *ev2-bsquote*
        (ev2-maybe-store
         `($fs-symbol ,(ev2-maker-form (symbol-name sym))
                      ,(ev2-maker-form (symbol-package sym)))
         store-index)
        (progn
          ;; Don't bother storing interned symbols, and all our symbols are interned
          (assert (or (eq (symbol-package sym) (symbol-package '$bs-quote))
                      (eq (symbol-package sym) *keyword-package*)))
          (when store-index
            (unless (or (eq sym 'bslambda)
                        (string= "$BS-" (string sym) :end2 4)
                        ;; Get rid of these
                        (string= "$FF-" (string sym) :end2 4))
              (format *trace-output* "~&NOT storing ~s" sym)
              (break "How did this find its way here?"))
            (remhash sym *ev2-fcomp-hash*))
          (if (keywordp sym) sym `(quote ,sym)))))))


(defun ev2-element-type-keyword (arr)
  (or (cadr (assoc (array-element-type arr)
                   '((character :simple-string)
                     (bit :bit-vector)
                     ((unsigned-byte 8) :unsigned-8-bit-vector)
                     ((unsigned-byte 16) :unsigned-16-bit-vector)
                     ((unsigned-byte 32) :unsigned-32-bit-vector)
                     ((unsigned-byte 64) :unsigned-64-bit-vector)
                     ((signed-byte 8) :signed-8-bit-vector)
                     ((signed-byte 16) :signed-16-bit-vector)
                     ((signed-byte 32) :signed-32-bit-vector)
                     ((signed-byte 64) :signed-64-bit-vector)
                     (double-float :double-float-vector)
                     (single-float :single-float-vector)
                     (fixnum :fixnum-vector)
                     ((complex single-float) :complex-single-float-vector)
                     ((complex double-float) :complex-double-float-vector)
                     (t :simple-vector))
                   :test 'equal))
      (error "unexpected array element type ~s" (array-element-type arr))))

(defun ev2-uvector-maker (type-key uvec store-index)
  (assert *ev2-bsquote*)
  `($fs-init-uvector ,(ev2-maybe-store
                       `($fs-make-uvector ,type-key ,(uvsize uvec))
                       store-index)
                     ,@(loop for i from 0 below (uvsize uvec)
                         collect (ev2-maker-form (uvref uvec i)))))

(defun ev2-simple-vector-maker (vector store-index)
  (check-type vector simple-vector)
  (cond (*ev2-bsquote*
         (ev2-uvector-maker :simple-vector vector store-index))
        (t
         (assert (not store-index)) ;; could support but not needed
         `(vector ,@(map 'list #'ev2-maker-form vector)))))

(defun ev2-ivector-maker (vector store-index)
  (assert *ev2-bsquote*)
  (check-type vector ivector)
  (ev2-uvector-maker (ev2-element-type-keyword vector) vector store-index))

;; It's not unusual to have 2-dim array immediates...  But maybe not in CCL sources?
(defun ev2-array-maker (arr store-index)
  (assert *ev2-bsquote*)
  (check-type arr simple-array)
  (let ((type (array-element-type arr)))
    (assert (or (eq type t) (subtypep type '(or number character)))))
  (let* ((type-key (ev2-element-type-keyword arr))
         (dims (array-dimensions arr)))
    `($fs-init-array ,(ev2-maybe-store `($fs-make-array ,type-key ',dims) store-index)
                     ,@(loop for i from 0 below (array-total-size arr) 
                         collect (ev2-maker-form (row-major-aref arr i))))))

(defun ev2-istruct-maker (istruct store-index)
  (assert *ev2-bsquote*)
  ;; Assume istruct layout is the same everywhere.
  (ev2-uvector-maker :istruct istruct store-index))

(defun ev2-cons-maker (cons store-index)
  (cond ((eq (car cons) cfasl-load-time-eval-sym)
         (assert *ev2-bsquote*)
         (destructuring-bind (form) (cdr cons)
           (ev2-maybe-store
            (if (funcall-lfun-p form)
              `($fs-funcall ,(ev2-maker-form (cadr form)))
              `($fs-eval ,(ev2-maker-form form)))
            store-index)))
        ((istruct-cell-p cons)
         (assert *ev2-bsquote*)
         (check-type (car cons) symbol)
         (ev2-maybe-store `($fs-istruct-cell ,(ev2-maker-form (car cons))) store-index))
        ((and (not *ev2-bsquote*) (eq (car cons) '$BS-QUOTE))
         (assert (and (cdr cons) (not (cddr cons))))
         (assert (not (gethash (cdr cons) *ev2-fcomp-hash*)))
         (let ((val-maker (let ((*ev2-bsquote* t))
                            (ev2-maker-form (cadr cons)))))
           (if store-index
             `(rplacd ,(ev2-maybe-store '(list '$BS-QUOTE) store-index) (list ,val-maker))
             `(list '$BS-QUOTE ,val-maker))))
        (store-index
         `(rplacd (rplaca ,(ev2-maybe-store '(cons nil nil) store-index)
                          ,(ev2-maker-form (car cons)))
                  ,(ev2-maker-form (cdr cons))))
        (t (let* ((rest cons)
                  (val-forms (loop collect (ev2-maker-form (pop rest))
                               while (and (consp rest) (not (gethash rest *ev2-fcomp-hash*))))))
             `(list* ,@val-forms ,(ev2-maker-form rest))))))

(defun ev2-function-maker (fn store-index)
  (assert *ev2-bsquote*)
  (let ((bslambda (ev2-lfun-bslambda fn)))
    ;;; ***TODO: Currently we're not generating/tracking the lfun-bits!!!
    ;;; Stick them in the bslambda, since can't give the lfun incorrect lfun-bits!
    ;;; Or have an XFUNCTION type that we use.
    (let ((*ev2-bsquote* nil))
      `($fs-init-function ,(ev2-maybe-store '($fs-cons-function) store-index)
                          ($fs-init-bslambda ,(ev2-maker-form bslambda))
                          ,(ev2-maker-form (lfun-bits fn))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
#+compile-boostrap (progn

;(require 'xfasload "ccl:xdump;xfasload.lisp")

;(rebuild-ccl :reload nil)
;; load up all the functions we're going to patch
;(xload-level-0)


;; rebuild-ccl -> compile-ccl, then xload-level-0 .
;; then if :RELOAD (default T), runs the kernel with
;;    --image-name (standard-boot-image-name) input "(save-application <standard-image-name>)"
;;  XLOAD-LEVEL-0:
;;   in compile-ccl.lisp, xload-level-0 is defined as (require-modules (target-xload-modules)) and then
;;     call the xload-level-0 that got defined in xfasload.lisp
;;   That will (target-Xcompile-level-0 target (eq recompile :force)) and then build the actual image!
;;; (xload-level-0)

(defvar *ok-to-bscompile* nil)

#|
(defun xload-level-0 (&optional (recompile t))
  (target-xload-level-0 (backend-name *host-backend*) recompile))

(target-xload-level-0 (target &optional (recompile t))
  (when recompile
       (target-Xcompile-level-0 target (eq recompile :force)))

|#
(defparameter *bscompile-system* '("level-1;bs-level-1"
                                   "level-1;l1-cl-package"
                                   "level-1;l1-utils"
                                   "level-1;l1-init"
                                   "level-1;l1-symhash"
                                   "level-1;l1-numbers"
                                   "level-1;l1-aprims"
                                   #+ppc-target "level-1;ppc-callback-support"
                                   #+x86-target "level-1;x86-callback-support"
                                   #+arm-target "level-1;arm-callback-support"
                                   ;; There is some problem with %pascal-functions% being evaluated, it keeps calling back.
                                   #+not-yet "level-1;l1-callbacks"
                                   #+not-yet "level-1;l1-sort"
                                   #+not-yet "lib;lists"
                                   #+not-yet "lib;sequences"
                                   #+not-yet "level-1;l1-dcode"
                                   #+not-yet "level-1;l1-clos-boot"
                                   #+not-yet "lib;hash"
                                   #+not-yet "level-1;l1-clos"
                                   #+not-yet "lib;defstruct"
                                   #+not-yet "lib;dll-node"
                                   #+not-yet "level-1;l1-unicode"
                                   #+not-yet "level-1;l1-streams"
                                   #+not-yet "level-1;linux-files"
                                   #+not-yet "lib;chars"
                                   #+not-yet "level-1;l1-files"
                                   #+not-yet "level-1;l1-typesys"
                                   
                                   #+not-yet "level-1;sysutils"
                                   ;#+not-yet #+ppc-target "level-1;ppc-threads-utils"
                                   ;#+not-yet #+x86-target "level-1;x86-threads-utils"
                                   ;#+not-yet #+arm-target "level-1;arm-threads-utils"
                                   #+not-yet "level-1;l1-lisp-threads"
                                   #+not-yet "level-1;l1-application"
                                   #+not-yet "level-1;l1-processes"
                                   #+not-yet "level-1;l1-io"
                                   #+not-yet "level-1;l1-reader"
                                   #+not-yet "level-1;l1-readloop"
                                   #+not-yet "level-1;l1-readloop-lds"
                                   #+not-yet "level-1;l1-error-system"
                                   
                                   #+not-yet "level-1;l1-events"
                                   ;#+not-yet #+ppc-target "level-1;ppc-trap-support"
                                   ;#+not-yet #+x86-target "level-1;x86-trap-support"
                                   ;#+not-yet #+arm-target "level-1;arm-trap-support"
                                   #+not-yet "level-1;l1-format"
                                   #+not-yet "level-1;l1-sysio"
                                   #+not-yet "level-1;l1-pathnames"
                                   #+not-yet "level-1;l1-boot-lds"
                                   
                                   #+not-yet "level-1;l1-boot-1"
                                   #+not-yet "level-1;l1-boot-2"
                                   #+not-yet "level-1;l1-boot-3"
                                   ))


(let* ((dir-path (truename "ccl:"))
       (dir-host (pathname-host dir-path))
       (dir-device (pathname-device dir-path))
       (dir-dirlist (pathname-directory dir-path))
       (dir-depth (length dir-dirlist)))

  (defun bscompile-this-file? (file)
    (when *ok-to-bscompile*
      (let ((file-path (truename file)))
        (and (equal (pathname-host file-path) dir-host)
             (equal (pathname-device file-path) dir-device)
             (let ((file-dirlist (pathname-directory file-path)))
               (and (>= (length file-dirlist) dir-depth)
                    (loop for dir-part in dir-dirlist for file-part = (pop file-dirlist)
                      always (equalp dir-part file-part))
                    (PROGN
                      (when (search "level-1" (pathname-name file) :test 'equalp)
                        (format t "~&bscompile ~s: ~s ~s => ~s"
                                file file-dirlist
                                (loop for target in *bscompile-system*
                                  collect (and (equalp file-dirlist (cdr (pathname-directory target)))
                                               (equalp (pathname-name file-path) (pathname-name target))))
                                (or (equal (car file-dirlist) "level-0")
                                    (loop for target in *bscompile-system*
                                      thereis (and (equalp file-dirlist (cdr (pathname-directory target)))
                                                   (equalp (pathname-name file-path) (pathname-name target)))))))
                    (or (equal (car file-dirlist) "level-0")
                        (loop for target in *bscompile-system*
                          thereis (and (equalp file-dirlist (cdr (pathname-directory target)))
                                       (equalp (pathname-name file-path) (pathname-name target)))))))))))))

;#+PATCH
(progn
  (defun bscompile-fasl () #P".bs-dx64fsl")


;; Once this is all settled, maybe could bind *.fasl-pathname*, but can't now because some files
;; use bscompile and some don't.

  ;(require'x8664env "ccl:lib;x8664env.lisp")
  (unless (fboundp 'setup-xload-target-parameters)
    (xload-level-0))

  (defun test-it (&key (verbose t))
    (load "ev:bscompile.lisp")
    (let* ((*features* (cons :cross-compiling *features*))
           (*standard-output* (if verbose *standard-output* (make-broadcast-stream)))
           (*ok-to-bscompile* t)
           ;; TODO: still need this??
           (*%fasload-verbose* nil)
           (*ccl-system* (let ((old *ccl-system*))
                           (assert (eq (caar old) 'level-1))
                           (cons '(LEVEL-1 "ccl:ccl;bs-level-1" ("ccl:l1;bs-level-1.lisp")) (cdr old)))))
      (flet ((bfasls (path)
               (directory (make-pathname :name :wild
                                         :type (pathname-type (bscompile-fasl))
                                         :defaults path))))
        (map nil 'delete-file (bfasls "ccl:level-0;**;"))
        (map nil 'delete-file (bfasls "ccl:l1-fasls;"))
        (map nil 'delete-file (bfasls "ccl:")))
      ;; rebuild-ccl, except we want to force compile level-0 but not anything else
      (let* ((*build-time-optional-features* nil)
             (*save-source-locations* nil)
             (cd (current-directory))
             (*cerror-on-constant-redefinition* nil)
             (*warn-if-redefine-kernel* nil))
        (unwind-protect
            (with-global-optimization-settings ()
              (setf (current-directory) "ccl:")
              (compile-file "ev:bseval.lisp" :output-file "ev:bseval.dx64fsl")
              (compile-ccl nil)
              (xload-level-0 :force))
          (setf (current-directory) cd)))))



(unadvise compile-named-function :name bscompile)
(unadvise x862-compile :name bscompile)
(unadvise find-module :name bscompile)
(unadvise compile-file :name bscompile)
(unadvise setup-xload-target-parameters :name bscompile)
(unadvise find-backend :name bscompile)
(unadvise xfasload :name bscompile)

  (defvar *use-bscompile-now* nil)

  ;; want definitions in level-0 compiled with fcomp-named-function to use bscompile.
  (unadvise compile-named-function :name bscompile)
  (advise compile-named-function
          (let ((*use-bscompile-now* (and *compiling-file*
                                          ;; Only time we have a load-time-eval-token is when called form
                                          ;; fcomp-named-function or from nx1-load-time-value, exactly the
                                          ;; two cases we want to intercept.
                                          (not (eq (getf (cdr arglist) :load-time-eval-token 'no) 'no))
                                          ;; only fcomp-named-function calls us with both :policy & :target
                                          #+old (and (not (eq (getf (cdr arglist) :policy 'no) 'no))
                                                     (not (eq (getf (cdr arglist) :target 'no) 'no)))
                                          (bscompile-this-file? *compiling-file*))))

            (:do-it))
          :when :around :name bscompile)

  (unadvise x862-compile :name bscompile)
  (advise x862-compile
          (if *use-bscompile-now*
            (apply #'ev2-compile arglist)
            (:do-it))
          :when :around :name bscompile)


  
  (defun bscompile-boot-image (&optional (xl-backend *xload-default-backend*))
    (let* ((boot-image (backend-xload-info-default-image-name xl-backend))
           (n (1+ (position-if (lambda (c) (find c ":;")) boot-image :from-end t))))
      (format t "~&current boot-image: ~s" boot-image)
      (concatenate 'string (subseq boot-image 0 n) "bs-" (subseq boot-image n))))

  (unadvise find-module :name bscompile)
  (advise find-module
          (destructuring-bind (fasl sources) values
            (when fasl
              (when (some #'bscompile-this-file? sources)
                (assert (every #'bscompile-this-file? sources))
                (setq values (list (merge-pathnames (bscompile-fasl) fasl) sources)))))
          :when :after :name bscompile)

  ;; TODO: this could be a :before
  ;; This is still needed because level-0 doesn't go through find-module
  (unadvise compile-file :name bscompile)
  (advise compile-file
          (if (bscompile-this-file? (car arglist))
            ;; instead of using (backend-target-fasl-pathname backend)
            (let ((output-file (or (getf (cdr arglist) :output-file)
                                   (make-pathname :type nil :defaults (car arglist)))))
              ;; find-module should have intervened for non-level-0 files.
              (ASSERT (or (equal (pathname-type output-file) (pathname-type (bscompile-fasl)))
                          (find "level-0" (pathname-directory output-file) :test 'equalp)))
              (setq arglist (list* (car arglist)
                                   :output-file
                                   (merge-pathnames (bscompile-fasl) output-file)
                                   (cdr arglist)))
              (:do-it))
            (:do-it))
          :when :around :name bscompile)

  ;; Now need to use to new fasls while loading, that's harder because it's just inlined in target-xload-level-0
  ;; Major kludge
  (defvar *bscompile-target-backend* nil)
  (defvar *start-patching-find-backend* nil)
  (unadvise setup-xload-target-parameters :name bscompile)
  (advise setup-xload-target-parameters
          (when *ok-to-bscompile* ;; always at least level 0, so patch this.
            (let ((bs-xl (copy-backend-xload-info *xload-target-backend*)))
              (setf (backend-xload-info-DEFAULT-IMAGE-NAME bs-xl)
                    (bscompile-boot-image bs-xl))
              (setf (backend-xload-info-DEFAULT-STARTUP-FILE-NAME bs-xl)
                    (let ((name (backend-xload-info-default-startup-file-name bs-xl)))
                      (assert (equal (pathname-name name) "level-1"))
                      (concatenate 'string "bs-" (pathname-name name) "." (pathname-type (bscompile-fasl)))))
              (setq *xload-target-backend* bs-xl)
              ;; Already fetched
              (setq *xload-startup-file* (backend-xload-info-default-startup-file-name bs-xl))
              ;; Patching fasl file name more complicated...
              (setq *start-patching-find-backend* t)))
          :when :after :name bscompile)
  (unadvise find-backend :name bscompile)
  (advise find-backend
          (if (shiftf *start-patching-find-backend* nil)
            (progn
              (setq *start-patching-find-backend* nil)
              (assert (eq (car arglist) (backend-xload-info-compiler-target-name *xload-target-backend*)))
              (let ((backend (copy-backend (:do-it))))
                (setf (backend-target-fasl-pathname backend) (bscompile-fasl))
                backend))
            (:do-it))
          :when :around :name bscompile)


  ;; Have to include compiled evaluator if bscompiled.
  (unadvise xfasload :name bscompile)
  (advise xfasload
          (when *ok-to-bscompile*
            (format t "~&Adjoining xfasload list")
            (when (find "bseval" arglist :test #'equalp :key #'pathname-name)
              (break "Bug, probably doubly-advised!"))
            (setq arglist (list* (car arglist)
                                 (truename "ev:bseval.dx64fsl")
                                 (cdr arglist))))
          :when :before :name bscompile)
)) ;; #+compile-bootstrap
|#

#|
#+CROSS (progn
(defun test-it ()
  (cross-xload-level-0 (backend-name *bseval-host-backend*) t))

(defparameter *bseval-host-backend*
  (let* ((backend (copy-backend *host-backend*))
         (name (backend-name backend))
         (bs-name (make-keyword (concatenate 'string (string name) "-BSEVAL")))
         (bs-fasl (bscompile-fasl))
         (bs-backend (copy-backend backend)))
    (setf (backend-p2-compile bs-backend) 'ev2-compile)
    (setf (backend-name bs-backend) bs-name)
    (setf (backend-target-fasl-pathname bs-backend) bs-fasl)
    (let* ((xl (find-xload-backend name))
           (bs-xl (copy-backend-xload-info xl)))
      (setf (backend-xload-info-name bs-xl) bs-name)
      (setf (backend-xload-info-compiler-target-name bs-xl) bs-name)
      (setf (backend-xload-info-default-image-name bs-xl)
            (let* ((image (backend-xload-info-default-image-name bs-xl))
                   (n (1+ (position-if (lambda (c) (find c ":;")) image :from-end t))))
              (concatenate 'string (subseq image 0 n) "bs-" (subseq image n))))
      ;; Really should just be the name, with type defaulting!
      (setf (backend-xload-info-default-startup-file-name bs-xl)
            (concatenate 'string "bs-level-1." (pathname-type bs-fasl)))
      (add-xload-backend bs-xl))
    (remove bs-name *known-backends* :key #'backend-name)
    (push bs-backend *known-backends*)
    bs-backend))
) ;;#+CROSS
|#



;  (trace  :before (lambda (fn afunc &rest flags) (assert (eq fn 'x862-compile)) flags (setq *last-afunc afunc)) x862-compile)
;; TODO:  pass the "vreg" arg in, it says whether it's being evaluated for a vlaue, and it's really useful
;;(fcomp-file src (or compile-file-original-truename (namestring orig-src)) compile-file-original-buffer-offset lexenv)


(defvar *ev2-cur-afunc*)

(defvar *ev2-lex-vars*)

(defun ev2-lex-var-p (var)
  (check-type var var)
  (or (ev2-var-inherited-from var)
      (not (logbitp $vbitspecial (nx-var-bits var)))))

(defun ev2-var-inherited-from (v)
  (unless (fixnump (var-bits v)) (var-bits v)))

(defun ev2-quote (obj)
  `($bs-quote ,obj))

(defun assign-misc-vcell (thing)
  (assert (or (consp thing) (vectorp thing))) ;; block is cons, tagbody is vector, temp vars are cons too.
  ;; I think this is ok, if have multiple blocks
  (assign-lex-vcell thing t))
  
(defun assign-lex-vcell (var &optional misc-p)  ;; var can also be a block tag
  (unless misc-p (assert (ev2-lex-var-p var)))
  (when (assoc var *ev2-lex-vars*) (error "~s already assigned" var))
  (let ((index (length *ev2-lex-vars*)))
    (push (cons var index) *ev2-lex-vars*)
    index))

(defun get-lex-vcell (var &optional misc-p)
  (unless misc-p (assert (ev2-lex-var-p var)))
  (cdr (or (assoc var  *ev2-lex-vars*)
           (error "Unknown lex var ~s" var))))

(defun get-misc-vcell (tag)
  (assert (or (consp tag) (vectorp tag)))
  (get-lex-vcell tag t))


(defparameter *ev2-prototype* (nlambda ev2-func (&rest args)
                                (bseval-apply-lambda nil 'ev2-lambda args)))
#+x86-target ;; not used, for debugging
(progn

(defparameter *ev2-lfun-bslambda-index*
  (let* ((fv (function-to-function-vector *ev2-prototype*))
         (idx (uvsize fv)))
    (loop when (eq (uvref fv (decf idx)) 'ev2-lambda) return idx)))

(defun ev2-lfun-bslambda (lfun)
  (let ((fv (function-to-function-vector (require-type lfun 'function))))
    (assert (eq (uvsize fv) (uvsize (function-to-function-vector *ev2-prototype*))))
    (let ((lambda (uvref fv *ev2-lfun-bslambda-index*)))
      (assert (eq (car lambda) 'bslambda))
      lambda)))
)

;;create-x86-function
#+x86-target
(defun ev2-make-lfun (fname lambda-sexp)
  (let* ((pfn *ev2-prototype*)
         (code-words (%function-code-words pfn)) ;; 14
         (pfv (function-to-function-vector pfn))
         (size (uvsize pfv))
         (fv (allocate-typed-vector :function size)))
    (%copy-ivector-to-ivector pfv 0 fv 0 (ash code-words target::word-shift))
    ;; TODO: flush source location info...
    (loop for i from code-words below size
      do (uvset fv i (uvref pfv i)))
    (flet ((offs (fv obj)
             (loop for i upfrom code-words below size
               when (eq (uvref fv i) obj) return i
               finally (error "didn't find ~s" obj))))
      (uvset fv (offs fv 'ev2-func) fname)
      (uvset fv (offs fv 'ev2-lambda) lambda-sexp))
    (function-vector-to-function fv)))

;;; (defun bseval-apply-lambda (&rest stuff) (format t "~&called with: ~s" stuff))


(defun ev2-compile (afunc &optional lambdaform record-symbols)  ;;x862-compile
  (dolist (a (afunc-inner-functions afunc))
    (unless (afunc-lfun a)
      (assert (eq (afunc-parent a) afunc))
      (ev2-compile a (if lambdaform (afunc-lambdaform a)) record-symbols)))
  #+NO  (FORMAT T "~&Compiling ~s, inh ~s vcells: ~s, fcells: ~s"  afunc
          (afunc-inherited-vars afunc)
          (afunc-vcells afunc)
          (afunc-fcells afunc))
  #+NO (pprint (decomp-acode (afunc-acode afunc)))
  #+NO (push afunc *AF)
  ;(when (afunc-vcells afunc) (break "What to do about vcells? ~s" (afunc-vcells afunc)))
  ;(when (afunc-fcells afunc) (break "What to do about fcells? ~s" (afunc-fcells afunc)))
  (let ((acode (afunc-acode afunc))
        (inherited-vars (afunc-inherited-vars afunc)))
    (assert (eq (acode-operator-sym acode) 'lambda-list))
    (when  inherited-vars
      (assert (afunc-parent afunc))
      (assert (let* ((outer-afunc (afunc-parent afunc))
                     (outer-vars (afunc-all-vars outer-afunc))
                     (outer-inh (afunc-inherited-vars outer-afunc)))
                (every (lambda (v)
                         (let ((outer-var (ev2-var-inherited-from v)))
                           (and outer-var
                                (or (member outer-var outer-vars)
                                    (member outer-var outer-inh)))))
                       inherited-vars))))
     (let ((bslambda 
           (let ((*ev2-cur-afunc* afunc))
             (apply #'ev2-lambda-form inherited-vars (acode-operands acode)))))
      (setf (afunc-lfun afunc)
            (ev2-make-lfun (afunc-name afunc) bslambda)))
    ;; now that we have an lfun, fixup any forward refs to the fn.
    (loop for ref in (afunc-fwd-refs afunc)
      do (assert (equal ref `($bs-quote ,afunc)))
      do (setf (cadr ref) (afunc-lfun afunc))))
  afunc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ev2-specials


;; This will become a vector like *x862-specials*, but for now.
(defvar *ev2-specials* (make-hash-table :test 'eq))

(defun acode-operator-sym (x)
  (acode-operator-name (if (acode-p x) (acode-operator x) x)))

(defun ev2-operator-function (acode)
  ;(svref *ev2-specials* (%ilogand #.operator-id-mask (acode-operator acode)))
  (or (gethash (acode-operator-sym acode) *ev2-specials*)
      (progn
        (error "Unknown operator ~s ~s" (acode-operator-sym acode)
               (acode-operands acode)))))

(defun ev2-form (acode)
  (if (nx-null acode)
    (ev2-quote nil)
    (if (nx-t acode)
      (ev2-quote t)
      ;; (and (null vreg) (%ilogbitp operator-acode-subforms-bit op) (%ilogbitp operator-assignment-free-bit op) (%ilogbitp operator-side-effect-free-bit op))
      ;; if the form is assignment free and side effect free, and not being evaluated for value, then can just eval the arguments.
      ;; (dolist (arg (acode-operators form)) (x862-form arg))
      (apply (ev2-operator-function acode) (acode-operands acode)))))

(defun ev2-arglist-forms (arglist)
  (destructuring-bind (stack-args revreg-args) arglist
    (append stack-args (reverse revreg-args))))

(defmacro defev2 (operator-name-or-names arglist &body forms)
  (multiple-value-bind (body decls) (parse-body forms nil t)
    `(progn
       ,@(if (consp operator-name-or-names)
           (loop for operator in operator-name-or-names
             collect `(record-source-file ',operator 'bscompile-operator))
           (list `(record-source-file ',operator-name-or-names 'bscompile-operator)))
       (let ((fn (nfunction (bscompile-operator ,operator-name-or-names)
                            (lambda ,arglist
                              ,@decls 
                              (block ,(if (consp operator-name-or-names)
                                        (car operator-name-or-names)
                                        operator-name-or-names)
                                ,@body)))))
         ,@(if (consp operator-name-or-names)
             (loop for operator-name in operator-name-or-names
               nconc (list #+NO`(when (gethash ',operator-name *ev2-specials*)
                              (format t "~&Replacing defn of ~s" ',operator-name))
                           ; (svset *x862-specials* (%ilogand #.operator-id-mask (%nx1-operator ,operator-name)) ,fun)
                           `(setf (gethash ',operator-name *ev2-specials*) fn)))
             (list #+NO `(when (gethash ',operator-name-or-names *ev2-specials*)
                      (format t "~&Replacing defn of ~s" ',operator-name-or-names))
                   ; (svset *x862-specials* (%ilogand #.operator-id-mask (%nx1-operator ,operator-name)) ,fun)
                   `(setf (gethash ',operator-name-or-names *ev2-specials*) fn)))))))

(defmacro defev2-fn (operator arglist runtime-op)
  (assert (every (lambda (x) (and (symbolp x) (not (eql #\& (char (string x) 0))))) arglist))
  (let ((ev2-args (mapcar (lambda (arg) `(ev2-form ,arg)) arglist)))
    `(defev2 ,operator ,arglist
       (list ',runtime-op ,@ev2-args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; binding things

(defun ev2-binding-var (var)
;;  (assert (fixnump (nx-var-bits (require-type var 'var)))) ;; not inherited
  (if (ev2-lex-var-p var)
    (assign-lex-vcell var)
    (var-name var)))

;;; Ok, so as we compile a lambda, all its vars get assigned a location in a vector
;;; that's going to be created on entry.  the locations will contain a cons that will be
;;;  the value cell of the variable.

;;; Ok,  but as we're compiling the inner function, we might have some refs to variables in outer
;;; function.  we have a VAR that is an inherited var, that points to the VAR in the outer function,
;;; but we don't know what vcell it will be assigned.  So we will not be accessing them from the
;;;  OK, the way this will work, is that WE will take the set of value cells as the initial args,
;;;    and will assign SLOTS IN OUR vars vector, and on entry to function, for inherited ones,
;;;    we will store the vcell.   ONCE entry is completed, the vector

;;;

;;;; *** AFUNC-ALL-VARS has all the vars we create, including internal LET's..  At compile time they get
;;;;   assigned an offset.  At run time, the offset will get a VALUE-CELL created when it gets bound.
;;;;  When this function is referenced, it will call (MAKE-CLOZURE, which will at runtime
;;;;   look up the inherited var VALUE CELLS and arrange to pass them in when called.

;;;;    so MAKE-CLOSURE <Inner-FUNC>.  Loop through inner func's inherited vars.
;;;;     these are the vars we want to get
;;;; So have inherited arg, look up with VAR-EA of the parent var.
;;;;  SO Innher-func has been compiled, and now we're compiling the parent function.  So WE CAN LOOK UP THE
;;;; VALUE CELL IN OUR VECTOR  ($lex-ref <parent-var>)

;;; (nfunction ,name (lambda (&lap 0) (x86-lap-function ,name ,arglist ,@body)))

(defun ev2-lambda-form (inh req opt rest keys auxen acode p2decls &aux lexpr)
;; ok, x862 stores the var location in VAR-LREG
  (declare (ignorable p2decls)) ;; stuff like tail-call-allow, safety, trust-declarations.
  (assert (every #'ev2-var-inherited-from inh))
  (assert (not (and (consp (car req)) (eq (caar req) '&lap))))
  ;;; **** TODO we currently lose the afunc bits totally, need to fix this!!!
  ;;; **** Need to compute the $LFBITS- for the function.
  ;;;(assert (not (logbitp $fbitmethodp (afunc-bits *ev2-cur-afunc*))))
  ;;; if methodp, first req is the method-var
  (when (consp rest)
    (assert (and (null opt) (null keys) (equal auxen '(nil nil))))
    (setq lexpr t rest (car rest))
    (assert (not (logbitp $vbitspecial (nx-var-bits rest)))))
  ;; why doesn't pass1 just handle this???
  (when auxen
    (destructuring-bind (vars vals) auxen
      (assert (= (length vars) (length vals)))
      (when vars
        (setq acode (make-acode (%nx1-operator let*) vars vals acode p2decls)))))
  (let ((*ev2-lex-vars* nil))
    ;; Ok, if have inherited, we're compiling an inner function.
    ;;   inherited vars are like [Local -> PARNET-VAR],
    ;;  S IT IS NOT INCLUDED IN ALL-VARS.  When called runtime value cells of the parent are going to
    ;; be passed in as first args.
    (prog1
      `(bslambda ,(ev2-quote (afunc-name *ev2-cur-afunc*))
                 (,(mapcar #'ev2-binding-var inh)
                  ,(mapcar #'ev2-binding-var req)
                  ,(when opt
                     (destructuring-bind (opt-vars opt-inits opt-supp-vars) opt
                       (assert (= (length opt-vars) (length opt-inits) (length opt-supp-vars)))
                       (mapcar (lambda (var init supp)
                                 (list (ev2-binding-var var)
                                       (EV2-FORM init)
                                       (and supp (ev2-binding-var supp))))
                               opt-vars opt-inits opt-supp-vars)))
                  ,(when rest (ev2-binding-var rest))
                  ,(when keys
                     (destructuring-bind (allow-other-keys-p keyvars keysupp keyinits keykeys) keys
                       (assert (= (length keyvars) (length keysupp) (length keyinits) (length keykeys)))
                       (cons allow-other-keys-p
                             (map 'list (lambda (key var init supp)
                                          (list (EV2-QUOTE key) ;; Need to quote it so gets converted
                                                (ev2-binding-var var)
                                                (EV2-FORM init)
                                                (and supp (ev2-binding-var supp))))
                                  keykeys keyvars keyinits keysupp)))))
                 ,(let* ((body (ev2-form acode)))
                    (if lexpr
                      (let ((rest-var (get-lex-vcell rest)))
                        (ev2-progn
                         `(($BS-LSET ,rest-var ($BS-LEXPR-ARGS ,rest-var))
                           ,body)))
                      body))
                 ,(length *ev2-lex-vars*))
      (assert (every #'(lambda (v) (or (member (car v) inh)
                                       (member (car v) (afunc-all-vars *ev2-cur-afunc*))
                                       (consp (car v)) ;; block tag
                                       (vectorp (car v)))) ;; tagbody
                     *ev2-lex-vars*)))))

;; Not clear why pass1 doesn't just handle this.
(defev2 lambda-bind (vals req rest keys-p auxen body p2decls)
  (assert (null keys-p)) ;; NIY
  (assert (<= (length req) (length vals)))
  (assert (or (null auxen)
              (destructuring-bind (vars vals) auxen
                (= (length vars) (length vals)))))
  (when auxen
    (destructuring-bind (vars vals) auxen
      (assert (= (length vars) (length vals)))
      (when vars
        (setq body (make-acode (%nx1-operator let*) vars vals body p2decls)))))
  (if rest
    (let ((nreq (length req))) ;; <= vals
      (assert (<= nreq (length vals)))
      (setq req `(,@req ,rest))
      (setq vals `(,@(subseq vals 0 nreq) ,(make-acode (%nx1-operator list) (nthcdr nreq vals)))))
    (assert (= (length req) (length vals))))
  (ev2-bind nil req vals body p2decls))

(defev2 let* (vars vals body p2decls) ;; x862-let*
  (ev2-bind t vars vals body p2decls))

(defev2 let (vars vals body p2decls) ;; x862-let
  (ev2-bind nil vars vals body p2decls))

(defev2 flet (vars afuncs body p2decls)
  (ev2-bind t vars (mapcar #'nx1-afunc-ref afuncs) body p2decls))

(defev2 labels (vars afuncs body p2decls)
  (ev2-bind nil vars (mapcar #'nx1-afunc-ref afuncs) body p2decls))

(defun ev2-bind (seq? vars vals body p2decls)
  (declare (ignore p2decls))
  (assert (eql (length vars) (length vals)))
  (let* ((bindings (loop for var in vars for val in vals
                     as bv = (ev2-binding-var var)
                     as init-form = (ev2-form val)
                     collect (list (if (and (logbitp $vbitdynamicextent (nx-var-bits var))
                                            ;;mostly don't bother, except we don't want to be consing
                                            ;; gc'able macptrs
                                            (eq (car init-form) '$bs-new-macptr))
                                     (list bv)
                                     bv)
                                   init-form)))
         (body-form (ev2-form body)))
    (cond ((null bindings) body-form)
          ((assoc '*interrupt-level* bindings)
           (assert (eql (length bindings) 1))
           `($BS-with-interrupt-level ,(cadr (car bindings)) ,body-form))
          ;; If there are no special variables, can treat a let as let*
          ((or seq? (loop for b in bindings never (symbolp (car b))))
           (when (eq (car body-form) '$BS-let*)
             (destructuring-bind (inner-bindings inner-form) (cdr body-form)
               (setq bindings (append bindings inner-bindings))
               (setq body-form inner-form)))
           (labels ((ssplit (bindings body-form)
                      (if (null bindings)
                        body-form
                        (let ((lex-bindings (loop while (and bindings (fixnump (car (car bindings))))
                                              collect (pop bindings))))
                          (if (null bindings)
                            `($BS-let* ,lex-bindings ,body-form)
                            (let* ((v (pop bindings))
                                   (body-form
                                    (if (consp (car v))
                                      `($BS-STACK-BLOCK ,(caar v) ,@(cdr (cadr v))
                                                        ,(ssplit bindings body-form))
                                      `($BS-progv ,(ev2-quote (list (car v))) ($BS-list ,(cadr v))
                                                  ,(ssplit bindings body-form)))))
                              (if (null lex-bindings)
                                body-form
                                `($BS-let* ,lex-bindings ,body-form))))))))
             (ssplit bindings body-form)))
          (t
           (let ((special-vars ())
                 (special-vals ()))
             (loop for b in bindings
               ;; mixing specials and stack block too hard...  Bet it never happens!
               do (assert (not (consp (car b))))
               unless (fixnump (car b)) do (let* ((temp (assign-misc-vcell b)))
                                             (push (car b) special-vars)
                                             (push `($BS-lref ,temp) special-vals)
                                             (setf (car b) temp)))
             (assert special-vars)
             `($BS-let* ,bindings
                        ($BS-progv ,(ev2-quote special-vars) ($BS-list ,@special-vals) ,body-form)))))))

(defev2 multiple-value-bind (vars val body p2decls)  ;x862-multiple-value-bind
  (declare (ignore p2decls))
  (assert (cdr vars)) ;; just to see if there's any reason to try to optimize this.
  `($BS-MULTIPLE-VALUE-BIND ,(mapcar #'ev2-binding-var vars) ,(ev2-form val) ,(ev2-form body)))

(defev2 multiple-value-prog1 (exprs)
  (assert exprs)
  (let ((valform (pop exprs)))
    (if exprs
      `($BS-MULTIPLE-VALUE-PROG1 ,(ev2-form valform)
                                ,(ev2-progn (mapcar #'ev2-form exprs)))
      (ev2-form valform))))

(defev2-fn progv (symbols values body) $BS-PROGV)

(defev2-fn multiple-value-list (form) $BS-MULTIPLE-VALUE-LIST)

(defev2-fn nth-value (n form) $BS-NTH-VALUE)

(defev2 values (forms)
  `($BS-values ,@(mapcar #'ev2-form forms)))

(defev2-fn unwind-protect (protected-form cleanup-form) $BS-UNWIND-PROTECT)

(defev2 lexical-reference (var)
  `($BS-LREF ,(get-lex-vcell var)))

(defev2 setq-lexical (var value)
  `($BS-LSET ,(get-lex-vcell var) ,(ev2-form value)))

(defev2 inherited-arg (arg) ;; x862-inherited-arg
  `($BS-VCELL-REF ,(get-lex-vcell arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;;;; calls, values

(defun ev2-progn (forms)
  (if (null forms)
    (ev2-quote nil)
    (let ((forms (loop for form in forms
                   when (eq (car form) '$BS-PROGN)
                   append (cdr form)
                   else collect form)))
      (if (cdr forms)
        `($BS-PROGN ,@forms)
        (car forms)))))


(defev2 progn (exprs) ;; x862-progn
  (ev2-progn (mapcar #'ev2-form exprs)))

;; Ugh, why doesn't pass1 just macroexpand it?  It's harder now because can't make a variable.
(defev2 prog1 (exprs)
  (assert exprs)
  `($BS-prog1 ,@(mapcar #'ev2-form exprs)))

(defev2 %decls-body (body p2decls)
  (declare (ignore p2decls))
  (ev2-form body))

(defun ev2-augmented-arglist (afunc arglist) ;; augmented with inherited variables
  (append (loop with outer = (afunc-inherited-vars *ev2-cur-afunc*)
            for var in (afunc-inherited-vars afunc)
            as root-var = (nx-root-var var)
            collect (make-acode (%nx1-operator inherited-arg)
                                (or (find root-var outer :key #'nx-root-var) root-var)))
          (ev2-arglist-forms arglist)))

(defev2 call (fn arglist &optional spread-p) ;; x862-call
  (assert (acode-p fn))
  `(,(if spread-p '$BS-apply '$BS-funcall)
    ,(ev2-form fn)
    ,@(mapcar #'ev2-form (ev2-arglist-forms arglist))))

(defev2 lexical-function-call (afunc arglist &optional spread-p)
  `(,(if spread-p '$BS-apply '$BS-funcall)
    ,(afunc-lfun-ref afunc)
    ,@(mapcar #'ev2-form (ev2-augmented-arglist afunc arglist))))

(defev2 self-call (arglist &optional spread-p)
  ;; Call back to the function being compiled.  %double-float does this.
  ;; also compile=named-function
  `(,(if spread-p '$BS-apply-lambda '$BS-funcall-lambda)
    ($BS-this-function) 
    ,@(mapcar #'ev2-form (ev2-augmented-arglist *ev2-cur-afunc* arglist))))

(defev2 multiple-value-call (fn-form arglist)
  `($BS-MVCALL ,(ev2-form fn-form) ,@(mapcar #'ev2-form arglist)))

(defev2 typed-form (type form &optional check-p)
  (when (equal type #+64-bit-target *nx-64-bit-fixnum-type* #+32-bit-target *nx-32-bit-fixnum-type*)
    (setq type 'fixnum))
  (if check-p
    `(,(or (cdr (assoc type '((fixnum . $BS-require-fixnum)
                              (cons . $BS-require-cons)
                              (list . $BS-require-list)
                              (symbol . $BS-require-symbol)
                              (integer . $BS-require-integer)
                              (gvector . $BS-require-gvector)
                              (number . $bs-require-number)
                              (real . $bs-require-real)
                              (character . $bs-require-character)
                              (simple-string . $bs-require-simple-string)
                              (simple-vector . $bs-require-simple-vector)
                              ((signed-byte 8) . $BS-require-s8)
                              ((unsigned-byte 8) . $BS-require-u8)
                              ((signed-byte 16) . $BS-require-s16)
                              ((unsigned-byte 16) . $BS-require-u16)
                              ((signed-byte 32) . $BS-require-s32)
                              ((unsigned-byte 32) . $BS-require-u32)
                              ((signed-byte 64) . $BS-require-s64)
                              ((unsigned-byte 64) . $BS-require-u64))
                       :test 'equal))
           (error "unsupported type ~s" type))
      ,(ev2-form form))
    (ev2-form form)))

;;;  **** TODO: now that we're not trying to bootstrap from nothing, this could all just be require-type

(defev2-fn require-fixnum (obj) $BS-require-fixnum)
(defev2-fn require-integer (obj) $BS-require-integer)
(defev2-fn require-number (obj) $BS-require-number)
(defev2-fn require-real (obj) $BS-require-real)
(defev2-fn require-character (obj) $BS-require-character)
(defev2-fn require-list (obj) $BS-require-list)
(defev2-fn require-symbol (obj) $BS-require-symbol)
(defev2-fn require-simple-string (obj) $BS-require-simple-string)
(defev2-fn require-simple-vector (obj) $BS-require-simple-vector)
(defev2-fn require-s8 (obj) $BS-require-s8)
(defev2-fn require-u8 (obj) $BS-require-u8)
(defev2-fn require-s16 (obj) $BS-require-s16)
(defev2-fn require-u16 (obj) $BS-require-u16)
(defev2-fn require-s32 (obj) $BS-require-s32)
(defev2-fn require-u32 (obj) $BS-require-u32)
(defev2-fn require-s64 (obj) $BS-require-s64)
(defev2-fn require-u64 (obj) $BS-require-u64)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; immediates
#+GZ
(defmethod print-object ((v var) stream)
  (print-unreadable-object (v stream :type t :identity t)
    (format stream "~s" (var-name v))
    (unless (fixnump (var-bits v))
      (format stream " inh ~s" (var-bits  v)))))

;; x862-fixnum 
(defev2 fixnum (value) (ev2-quote value))

(defev2 immediate (value) ;; x862-immediate
  ;; Don't really need to do anything special for loadtime value, it's communicated from
  ;; compiler pass1 to file-compiler.
  ;;(if (and (listp value) *load-time-eval-token* (eq (car value) *load-time-eval-token*)) ..)
  (ev2-quote value))

(defev2 simple-function (afunc) ;; x862-simple-function just does immediate for x862-afunc-lfun-ref
  (afunc-lfun-ref afunc))

(defun afunc-lfun-ref (afunc)
  (if (afunc-lfun afunc)
    (ev2-quote (afunc-lfun afunc))
    ;; This first happens in nx-record-code-coverage-acode
    (let ((ref (copy-list (ev2-quote afunc))))
      (push ref (afunc-fwd-refs afunc))
      ref)))


;; At runtime, this will create a function that does (apply inner (vcell 1) (vcell 2)  ... ARGS),
;; We can't do it here because can't create a new variable!!
;;;; * OR  MAYBE WE CAN?  can  bind misc.
(defev2 closed-function (inner-afunc)
  `($BS-CLOSED-FUNCTION ,(afunc-lfun-ref inner-afunc)
                        ,(loop for inner-var in (afunc-inherited-vars inner-afunc)
                           as var = (ev2-var-inherited-from inner-var)
                           do (assert (or (member var (afunc-all-vars *ev2-cur-afunc*))
                                          (member var (afunc-inherited-vars *ev2-cur-afunc*))))
                           collect (get-lex-vcell var))))

;; (test-fn '(lambda (a b) (list #'(LAMBDA (x) (+ x b)) a)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; global vars/fns

(defev2 (special-ref global-ref free-reference bound-special-ref) (sym) ;; x862-special-ref
  (if (eq sym '*interrupt-level*)
    '($BS-INTERRUPT-LEVEL)
    `($BS-SYMBOL-VALUE ($bs-quote ,sym))))

;;; *** RENAME TO $BS-SET-SYMBOL-VALUE
(defev2 (setq-special setq-free global-setq) (sym val)
  (check-type sym symbol)
  `($BS-SETQ-SPECIAL ($bs-quote ,sym)  ,(ev2-form val)))

(defev2 %function (sym)
  (check-type sym symbol)
  `($BS-SYMBOL-FUNCTION ($BS-QUOTE ,sym)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test, compare

(defun ev2-cc-operand (acode)
  (acode-immediate-operand acode))

;; Ops that return T or NIL.
(defun ev2-boolean-op-p (op)
  (member op '($BS-not $BS-yes $BS-eq $BS-ne $BS-gt $BS-le $BS-lt $BS-ge $BS-characterp $BS-endp
                      $BS-logbitp $BS-istruct-typep $BS-base-char-p $BS-macptr-eql)))

(defun ev2-boolean-form (cc op &rest args)
  (ecase (ev2-cc-operand cc)
    (:EQ `(,op ,@args))
    (:NE (if (eq op '$BS-not)
           (let ((arg (car args)))
             (if (ev2-boolean-op-p (car arg))
               arg
               `($BS-yes ,arg)))
           `($BS-not (,op ,@args))))))


(defev2 not (cc val) ;;x862-not
  (ev2-boolean-form cc '$BS-not (ev2-form val)))

(defun ev2-compare (cc form1 form2)
  `(,(ecase (ev2-cc-operand cc)
       (:EQ '$BS-EQ)
       (:NE '$BS-NE)
       (:GT '$BS-GT)
       (:LE '$BS-LE)
       (:LT '$BS-LT)
       (:GE '$BS-GE))
    ,(ev2-form form1)
    ,(ev2-form form2)))

;; lots of constant folding here see x862-eq-test
(defev2 (eq neq) (cc form1 form2)
  (assert (member (ev2-cc-operand cc) '(:EQ :NE)))
  (ev2-compare cc form1 form2))

(defev2 (numcmp short-float-compare double-float-compare %i<> %natural<>) (cc form1 form2) ;;x862-numcmp
  (ev2-compare cc form1 form2))

(defev2 int>0-p (cc form)
  (assert (eq (ev2-cc-operand cc) :gt))
  `($BS-GT ,(ev2-form form) ,(ev2-quote 0)))

(defev2 characterp (cc value) ;; x862-characterp
  (ev2-boolean-form cc '$BS-CHARACTERP (ev2-form value)))

(defev2 endp (cc form)
  (ev2-boolean-form cc '$BS-ENDP (ev2-form form)))

(defev2 consp (cc form)
  (ev2-boolean-form cc '$BS-CONSP (ev2-form form)))

(defev2 %ilogbitp (cc bitnum value)
  (assert (member (ev2-cc-operand cc) '(:eq :ne)))
  ;; For some reason, this one is reversed.
  (ev2-boolean-form cc '$BS-not `($BS-logbitp ,(ev2-form bitnum) ,(ev2-form value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; block, tagbody, catch

;; Pass1 converts blocks with no returns into progn, so we don't need to.
(defev2 local-block (blocktag body) ;; x862-local-block
  `($BS-BLOCK ,(assign-misc-vcell blocktag) ,(ev2-form body)))

(defev2 local-return-from (blocktag value) ;;x862-return-from
  `($BS-RETURN-FROM ,(get-misc-vcell blocktag) ,(ev2-form value)))

(defev2-fn catch (tag form) $BS-CATCH)
(defev2-fn throw (tag form) $BS-THROW)

(defev2 local-tagbody (taglist body) ;;x862-local-tagbody
  ;; a tag is (tag-sym inner-ref ref-count catch-var T bwd-p).  All of this is internal pass1
  ;; stuff except bwd-p, which is for us, and it's true if this tag was every the target
  ;; of a backward jump, i.e. an actual loop.   This was I think for explicit event checking.
  (let* ((counter (list 0))
         (exprs (loop with form-index = 0 for tag-or-expr in body
                  if (eq (acode-operator-sym tag-or-expr) 'tag-label)
                  do (let ((tag (car (acode-operands tag-or-expr))))
                       (assert (member tag taglist))
                       (setf (cadr tag) (list nil form-index counter)))
                  else if (eq (acode-operator-sym tag-or-expr) 'progn)
                  append (let ((subexprs (car (acode-operands tag-or-expr))))
                           (assert (eq (length (acode-operands tag-or-expr)) 1))
                           (incf form-index (length subexprs))
                           subexprs)
                  else collect tag-or-expr and do (incf form-index)))
         (codevec (make-array (length exprs)))
         (catch-tag-var (assign-misc-vcell codevec)))
    (loop for tag in taglist do (setf (car (cadr tag)) codevec))
    (flet ((target (form)
             (flet ((go-target (form)
                      (and (eq (car form) '$bs-go)
                           (destructuring-bind (tag-var target) (cdr form)
                             (when (eq tag-var catch-tag-var)
                               (decf (car counter))
                               target)))))
               (let ((target (go-target form)))
                 (if target
                   (values target nil)
                   (let* ((target (and (eq (car form) '$bs-progn) (go-target (car (last form))))))
                     (if target
                       (values target (let ((progn (butlast form)))
                                        (if (null (cddr progn)) (cadr progn) progn)))
                       (values nil form))))))))
      (loop for expr in exprs
        as form = (ev2-form expr)
        as i upfrom 0 do
        (setf (aref codevec i)
              (cond ((eq (car form) '$BS-go)
                     (let ((target (target form)))
                       (if target
                         `($BS-local-go ,target)
                         form)))
                    ((eq (car form) '$BS-IF)
                     (destructuring-bind (test yes no) (cdr form)
                       (multiple-value-bind (yes-target yes-form) (target yes)
                         (multiple-value-bind (no-target no-form) (target no)
                           (if (or yes-target no-target)
                             `($BS-LOCAL-GO-IF ,test ,yes-form ,yes-target ,no-form ,no-target)
                             form)))))
                    (t form))))
      ;; number-case generates a GO from deep within a case stmt.
      ;; verify-lambda-list has a GO to outer loop.
      ;(unless (eql 0 (car counter)) (FORMAT T "~&Have ~s missing $LOCAL-GO's" (car counter)))
      (if (eql 0 (car counter))
        `($BS-local-tagbody ,codevec)
        `($BS-tagbody ,catch-tag-var ,codevec)))))

(defev2 local-go (tag)
  (destructuring-bind (codevec form-index counter) (cadr tag)
    (incf (car counter))
    `($BS-GO ,(get-misc-vcell codevec) ,form-index)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; conses/uvectors

(defev2 (list %temp-list) (forms)
  `($BS-list ,@(mapcar #'ev2-form forms)))

(defev2 list* (arglist)
  ;; TODO: maybe should extract last arg here, so evaluator doesn't have to?
  `($BS-list* ,@(mapcar #'ev2-form (ev2-arglist-forms arglist))))

(defev2-fn (cons %temp-cons) (x y) $BS-CONS)

(defev2-fn make-list (size initial-element) $bs-make-list)

;; MIght need a separate $BS-%CAR form if this is ever used to cheat.  Maybe not though.
;; nx1-set-cxr compiles (set-car x y) into (%car (%rplaca x y)), which we should undo.
(defev2-fn (car %car) (cons) $BS-CAR)
(defev2-fn (cdr %cdr) (cons) $BS-CDR)
(defev2-fn (%rplacd rplacd) (cons value) $BS-RPLACD)
(defev2-fn set-cdr (cons value) $BS-SET-CDR)
(defev2-fn (%rplaca rplaca) (cons value) $BS-RPLACA)
(defev2-fn set-car (cons value) $BS-SET-CAR)


(defev2 %gvector (arglist) ;; x862-%gvector
  (let* ((args (ev2-arglist-forms arglist))
         (subtag-arg (pop args))
         (subtag (acode-fixnum-form-p subtag-arg)))
    `($BS-GVECTOR ,subtag ,@(mapcar #'ev2-form args))))

(defev2 vector (args)
  `($BS-GVECTOR ,(nx-lookup-target-uvector-subtag :simple-vector) ,@(mapcar #'ev2-form args)))

(defev2 %make-uvector (size subtag &optional (init nil init-p)) ;; x862-%alloc-misc
  (if init-p
    (let* ((subtag-val (acode-fixnum-form-p subtag)))
      (assert subtag-val)
      (if (member (nx-target-uvector-subtag-name subtag-val) (arch::target-gvector-types (backend-target-arch *target-backend*)))
        ;; Need to split this off for level-0
        `($BS-make-gvector-init ,subtag-val ,(ev2-form size) ,(ev2-form init))
        `($BS-make-ivector-init ,subtag-val ,(ev2-form size) ,(ev2-form init))))
    `($BS-make-uvector ,(ev2-form size) ,(ev2-form subtag))))

;; JUST use UVREF/UVSET for this?
(defev2-fn %svref (vec index) $BS-%SVREF) ;; need a special one so that can access internal vectors.
(defev2-fn %svset (vec index value) $BS-%SVSET)

(defev2-fn (uvref svref) (vec index) $BS-UVREF)
(defev2-fn (uvset svset) (vec index value) $BS-UVSET)
(defev2-fn uvsize (vec) $BS-UVSIZE)

(defev2 %typed-uvset (type uvector index newval)
  (let ((subtag (or (acode-fixnum-form-p type)
                     (nx-lookup-target-uvector-subtag
                      (acode-immediate-operand type)))))
    `($BS-SUBTAG-MISC-SET ,subtag ,(ev2-form uvector) ,(ev2-form index) ,(ev2-form newval))))

(defev2 %typed-uvref (type uvector index)
  (let ((subtag (or (acode-fixnum-form-p type)
                    (nx-lookup-target-uvector-subtag
                     (acode-immediate-operand type)))))
    `($BS-SUBTAG-MISC-REF ,subtag ,(ev2-form uvector) ,(ev2-form index))))
  

(defev2-fn aset1 (arr i val) $BS-ASET1)
(defev2-fn %aref1 (arr i) $BS-AREF1)

;; Can probably get by not implementing these for level-0 and then just call AREF!!
(defev2-fn general-aref2 (arr i j) $BS-AREF2) ;; x862-generic-aref2
(defev2-fn general-aref3 (arr i j k) $BS-AREF3) ;; really?
(defev2-fn general-aset2 (arr i j val) $BS-ASET2) ;; x862-general-aset2
(defev2-fn general-aset3 (arr i j k val) $BS-ASET3) ;; really?


(defev2-fn %scharcode (string index) $BS-%SCHARCODE) ;;x862-%scharcode
(defev2-fn %sbchar (string index) $BS-%SBCHAR)
(defev2-fn %set-sbchar (string index value) $BS-SET-%SBCHAR)
(defev2-fn %set-scharcode (string index value) $BS-SET-SCHARCODE)


;; this assumes the value is a lisp object, i.e. doesn't box it.
(defev2-fn %fixnum-ref (address offset) $BS-FIXNUM-REF)
;; This assumes the value is an natural unsigned word, and boxes it.
(defev2-fn %fixnum-ref-natural (address offset) $BS-FIXNUM-REF-NATURAL)
;; Value is an unsigned integer 64, gets unboxed & stored.
(defev2-fn %fixnum-set-natural (address offset value) $BS-FIXNUM-SET-NATURAL)


(defev2-fn %lisp-word-ref (vec index) $BS-LISP-WORD-REF)


(defev2-fn struct-set (struct offset val) $BS-STRUCT-SET)
(defev2-fn struct-ref (struct offset) $BS-STRUCT-REF)

(defev2-fn %slot-ref (instance idx) $BS-SLOT-REF)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Types

 ;; x862-lisptag
(defev2-fn typecode (node) $BS-TYPECODE)
(defev2-fn fulltag (node) $BS-FULLTAG)
(defev2-fn lisptag (node) $BS-LISPTAG)

(defev2-fn gvector-typecode-p (val) $BS-GVECTOR-TYPECODE-P)
(defev2-fn ivector-typecode-p (val) $BS-IVECTOR-TYPECODE-P)

(defev2 istruct-typep (cc object type-cell-form) ;;x862-istruct-typep
  (let ((type-cell (acode-immediate-operand type-cell-form)))
    (assert (and (consp type-cell) (symbolp (car type-cell))))
    (ev2-boolean-form cc '$BS-istruct-typep (ev2-form object) `($bs-quote ,(car type-cell)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; numbers, chars

(defev2-fn (add2 %short-float+-2 %double-float+-2 fixnum-add-overflow) (x y) $BS-ADD2)
(defev2-fn (sub2 %short-float--2 %double-float--2 fixnum-sub-overflow) (x y) $BS-SUB2)
(defev2-fn (mul2 %i* %short-float*-2 %double-float*-2) (x y) $BS-MUL2)
(defev2-fn (div2 %short-float/-2 %double-float/-2) (x y) $BS-DIV2)
(defev2-fn %iasr (shift x) $BS-IASR)
(defev2-fn %ilsr (shift x) $BS-ILSR)
(defev2-fn %ilsl (shift x) $BS-ILSL)
(defev2-fn (%ilogior2 logior2) (x y) $BS-LOGIOR2)
(defev2-fn (%ilogxor2 logxor2) (x y) $BS-LOGXOR2)
(defev2-fn logand2 (x y) $BS-LOGAND2)
(defev2-fn %ilogand2 (x y) $BS-%ILOGAND2)

(defev2-fn (%ilognot lognot) (x) $BS-LOGNOT)
(defev2-fn logbitp (x y) $BS-LOGBITP)
(defev2-fn %quo2 (x y) $BS-QUO2)
(defev2-fn (ash fixnum-ash) (x y) $BS-ASH) ;; fixnum-ash might rely on truncating

(defev2-fn (%single-float %fixnum-to-single) (arg) $BS-SINGLE-FLOAT)

(defev2-fn (%double-float %fixnum-to-double) (arg) $BS-DOUBLE-FLOAT)

(defev2-fn %setf-double-float (double val) $BS-SETF-DOUBLE-FLOAT)

(defev2-fn fixnum-sub-no-overflow (x y) $BS-%i-)
(defev2-fn fixnum-add-no-overflow (x y) $BS-%i+)

(defev2-fn %word-to-int (word) $BS-WORD-TO-INT)

(defev2-fn (char-code %char-code) (char) $BS-CHAR-CODE)

(defev2-fn (code-char %code-char %valid-code-char) (code) $BS-CODE-CHAR)

(defev2 base-char-p (cc char)
  (ev2-boolean-form cc '$BS-base-char-p (ev2-form char)))

;; See if this ever needs to cheat...
(defev2 (%%ineg %ineg minus1) (x) `($BS-SUB2 ,(ev2-quote 0) ,(ev2-form x)))

(defev2-fn (%complex-single-float-realpart %complex-double-float-realpart realpart)
  (arg) $BS-COMPLEX-REALPART)
(defev2-fn (%complex-single-float-imagpart %complex-double-float-imagpart imagpart)
  (arg) $BS-COMPLEX-IMAGPART)

(defev2 %make-complex-single-float (real imag)
  `($BS-MAKE-COMPLEX ,(ev2-quote 'single-float) ,(ev2-form real) ,(ev2-form imag)))

(defev2 %make-complex-double-float (real imag)
  `($BS-MAKE-COMPLEX ,(ev2-quote 'double-float) ,(ev2-form real) ,(ev2-form imag)))

(defev2 complex (real imag)
  `($BS-MAKE_COMPILEX ,(ev2-quote T) ,(ev2-form real) ,(ev2-form imag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; conditionals

(defev2-fn if (testform true false) $BS-IF)

(defev2 or (forms)
  `($BS-OR ,@(mapcar #'ev2-form forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MISC

(defev2 %err-disp (arglist) ;; x862-%err-disp
  (let ((args (ev2-arglist-forms arglist)))
    ;;(map nil 'print args)
    (ev2-progn `(($BS-SIGNALERR ,@(mapcar #'ev2-form args))
                 ,(ev2-quote nil)))))

(defev2 %badarg2 (badthing goodthing)
  `($BS-SIGNALERR ,(ev2-quote $XWRONGTYPE)
                  ,(ev2-form badthing)
                  ,(ev2-form goodthing)))

(defev2-fn %debug-trap (arg) $BS-DEBUG-TRAP)

(defev2-fn %symptr->symvector (symptr) $BS-SYMPTR-TO-SYMVECTOR)
(defev2-fn %symvector->symptr (symvector) $BS-SYMVECTOR-TO-SYMPTR)
(defev2-fn %symbol->symptr (symbol) $BS-SYMBOL-TO-SYMPTR)
  
(defev2-fn %current-tcr () $BS-CURRENT-TCR)

(defev2-fn %UNBOUND-MARKER () $BS-UNBOUND-MARKER)
(defev2-fn %SLOT-UNBOUND-MARKER () $BS-SLOT-UNBOUND-MARKER)
(defev2-fn %illegal-marker () $BS-ILLEGAL-MARKER)

(defev2-fn %current-frame-ptr () $BS-CURRENT-FRAME-PTR)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Foreign fns, macptrs

;; Have all these type htings be one.
;;(defx862 x862-characterp characterp (seg vreg xfer cc form)

;; HMM, who generates htis.
;;(defx862 x862-lisptag lisptag (seg vreg xfer node)
;;(defx862 x862-fulltag fulltag (seg vreg xfer node)

;;(defx862 x862-typecode typecode (seg vreg xfer node)

  ;; (x862-%immediate-store seg vreg xfer bits ptr offset val)
;; These will be replaced by integers once everything settles.  Make them negative so can
;; distinguish from memory block case.
;;;; *** TODO GET RID OF THIS
(defconstant $ff-signed8 '$ff-signed8)
(defconstant $ff-unsigned8 '$ff-unsigned8)
(defconstant $ff-signed16 '$ff-signed16)
(defconstant $ff-unsigned16 '$ff-unsigned16)
(defconstant $ff-signed32 '$ff-signed32)
(defconstant $ff-unsigned32 '$ff-unsigned32)
(defconstant $ff-signed64 '$ff-signed64)
(defconstant $ff-unsigned64 '$ff-unsigned64)
(defconstant $ff-single-float '$ff-single-float)
(defconstant $ff-double-float '$ff-double-float)
(defconstant $ff-fixnum '$ff-fixnum)
(defconstant $ff-address '$ff-address)
(defconstant $ff-void '$ff-void)

;; This is just to allow some constant folding...
(defev2 %macptrptr% (form)
  (ev2-form form))

(defev2 with-variable-c-frame (size body)
  ;; Undo what pass1 did...
  (assert (eq (acode-operator-sym body) 'let*))
  (destructuring-bind (vars vals let-body . ignore) (acode-operands body)
    (declare (ignore ignore))
    (assert (= (length vars) (length vals) 1))
    (let ((var (car vars)) (val (car vals)))
      (assert (eq (acode-operator-sym val) '%foreign-stack-pointer))
      (setq body let-body)
      `($BS-WITH-VARIABLE-C-FRAME ,(ev2-form size) ,(ev2-binding-var var) ,(ev2-form let-body)))))

(defev2 %foreign-stack-pointer ()
  (error "%foreign-stack-pointer not within with-variable-c-frame!"))


;; To avoid consing macptrs, pass1 deconstructs stuff like %int-to-ptr into a series of immediate
;; operations that then end up calling %CONSMACPTR% at the end.  For now re-construct that.
(defev2 %consmacptr% (arg) ;;x862-%consmacptr%
  (ecase (acode-operator-sym arg)
    (%immediate-int-to-ptr 
     `($BS-INT-TO-MACPTR ,@(mapcar #'ev2-form (acode-operands arg)))) ;; %int-to-ptr
     ;; Argh, should open code it, don't really need this
    (%immediate-inc-ptr
     `($BS-INC-MACPTR ,@(mapcar #'ev2-form (acode-operands arg))))
    (immediate-get-ptr
     `($BS-macptr-get ,@(mapcar #'ev2-form (acode-operands arg)) $ff-address))))

(defev2-fn %immediate-ptr-to-int (form) $BS-MACPTR-TO-INT)

(defev2 immediate-get-xxx (bits macptr offset)
  (let* ((fixnump (logbitp 6 bits))
         (signed (logbitp 5 bits))
         (size (logand 15 bits))
         (ffsize (if fixnump
                   $ff-fixnum
                   (ecase size
                     (8 (if signed $ff-signed64 $ff-unsigned64))
                     (4 (if signed $ff-signed32 $ff-unsigned32))
                     (2 (if signed $ff-signed16 $ff-unsigned16))
                     (1 (if signed $ff-signed8 $ff-unsigned8))))))
    `($BS-macptr-get ,(ev2-form macptr) ,(ev2-form offset) ,ffsize)))

(defev2 %get-double-float (macptr offset)
  `($BS-macptr-get ,(ev2-form macptr) ,(ev2-form offset) ,$ff-double-float))

(defev2 %get-single-float (macptr offset)
  `($BS-macptr-get ,(ev2-form macptr) ,(ev2-form offset) ,$ff-single-float))


(defev2-fn %new-ptr (size clear-p) $BS-NEW-MACPTR) ;x862-%new-ptr
  
(defev2 %immediate-int-to-ptr (arg)
  (error "%immediate-in-to-ptr Not supported: ~s" arg))

(defev2 %ptr-eql (cc form1 form2)
  (ev2-boolean-form cc '$BS-MACPTR-EQL (ev2-form form1) (ev2-form form2)))


(defev2-fn %setf-macptr (macptr val) $BS-SETF-MACPTR)


(defev2 %immediate-set-xxx (bits macptr offset val) ;x862-%immediate-set-xxx
  ;;(x862-%immediate-store seg vreg xfer bits ptr offset val)
  (let* ((size (logand #xF bits)) ;; 0 means ...
         (signed (not (logbitp 5 bits)))
         (ffsize (ecase size
                   (8 (if signed $ff-signed64 $ff-unsigned64))
                   (4 (if signed $ff-signed32 $ff-unsigned32))
                   (2 (if signed $ff-signed16 $ff-unsigned16))
                   (1 (if signed $ff-signed8 $ff-unsigned8))
                   (0 (assert signed) $ff-address))))
    `($BS-macptr-set ,(ev2-form macptr) ,(ev2-form offset) ,ffsize ,(ev2-form val))))

(defev2 %set-double-float (macptr offset val)
  `($BS-macptr-set ,(ev2-form macptr) ,(ev2-form offset) ,$ff-double-float ,(ev2-form val)))

(defev2 %set-single-float (macptr offset val)
  `($BS-macptr-set ,(ev2-form macptr) ,(ev2-form offset) ,$ff-single-float ,(ev2-form val)))

;;(mapcar #'foo '(6 8 4 12 10 22 21 11))
(defev2 builtin-call (index arglist);; x862-builtin-call
  ;; This is just an optimization to save space by having a subprim call the function
  ;;;  ******TODO: get rid of the special opcodes for these
  (let* ((args (ev2-arglist-forms arglist))
         (index-val (acode-fixnum-form-p index))
         (builtin (svref %builtin-functions% index-val))
         (op (cdr (assoc builtin '((>-2 . $BS-BUILTIN-GT)
                                   (<-2 . $BS-BUILTIN-LT)
                                   (=-2 . $BS-BUILTIN-EQ)
                                   (sequence-type . $BS-BUILTIN-SEQTYPE)
                                   (eql . $BS-BUILTIN-EQL)
                                   (ash . $BS-BUILTIN-ASH)
                                   (%aset1 . $BS-BUILTIN-ASET1)
                                   (%aref1 . $BS-BUILTIN-AREF1)
                                   (length . $BS-BUILTIN-LENGTH))))))
    (assert op () "Unknown builtin ~s" builtin)
    `(,op ,@(mapcar #'ev2-form args))))


(defev2 %reference-external-entry-point (arg)
  `($BS-%reference-external-entry-point ,(ev2-form arg)))


(defev2 ff-call (address argspecs argvals resultspec &optional monitor)
  (declare (ignore monitor))
  (assert (not (find :void argspecs)))
  (flet ((ffspec (spec)
           (case spec
             ((nil) (target-word-size-case
                     (64 $ff-signed64)
                     (32 $ff-signed32)))
             (:signed-byte $ff-signed8)
             (:unsigned-byte $ff-unsigned8)
             (:signed-halfword $ff-signed16)
             (:unsigned-halfword $ff-unsigned16)
             (:signed-fullword $ff-signed32)
             (:unsigned-fullword $ff-unsigned32)
             (:signed-doubleword $ff-signed64)
             (:unsigned-doubleword $ff-unsigned64)
             (:single-float $ff-single-float)
             (:double-float $ff-double-float)
             (:address $ff-address)
             (:void $ff-void)
             (t (require-type spec 'unsigned-byte)))))
    (let* ((argspecs (map 'list #'ffspec argspecs))
           (resultspec (ffspec resultspec))
           (address-form (ev2-form address))
           (arg-forms (list argspecs (map 'list #'ev2-form argvals) resultspec)))
      (assert (not (typep resultspec 'unsigned-byte)))
      #+NO (format t "~&FF argspecs: ~s => ~s~%" (remove-duplicates argspecs) resultspec)
      (if (and (eq (first address-form) '$bs-funcall)
               (equal (second address-form) '($bs-quote cvm-%kernel-import))
               (eq (car (third address-form)) '$bs-quote))
        `($BS-KERNEL-CALL ,(symbol-name (cadr (third address-form))) ,@arg-forms)
        (progn
          (unless (equal (car address-form) '$bs-symbol-value)
            (FORMAT *TRACE-OUTPUT* "~&FF-CALL ~s" (ev2-form address))
            (break "Different ff-call"))
          `($BS-FF-CALL ,address-form ,@arg-forms))))))

