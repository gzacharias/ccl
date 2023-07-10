(in-package :ccl)

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
    ;(bsload)
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

