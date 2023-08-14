(in-package :ccl)

(defparameter *modules-not-for-cvm*
  '(edit-callers
    cover
    leaks
    core-files
    dominance
    backtrace-lds ;; either make this load, or get rid of backtrace as well.
    vreg
    vinsn
    reg))

(import 'compile-cvm :cl-user)

;; So, this is called for cross-compiling cvm when it's running in some regular CCL.
;; It will cross compile and put output in ccl:cvmsrcs;
;;  this output should then get archived somewhere, like an IMAGE, and then will be used
;;   to load cvm into a VM.
;;   SO now have have ccl running where darwincvm is the host system.  We want to be
;;    able to rebuild it, incase the stuff ccl:cvmsrcs; got old.

;;    So there want to run native, and maybe rebuild-ccl?  this will put the "fasl" files in all the
;;     usual places it puts fasl files.  have to replace the xloading of level-0, but otherwise should be ok.
;;   Then instead of building an image, we put all the fasls in ccl:cvmsrcs; rebuild-ccl
;; HAVE TO put the cvmsrcs file elsewhere because rebuild-ccl :clean deletes ccl:**;*.<fasl>
;; So ok, it does COMPILE-CCL, then XLOAD-LEVEL-0,
;;;   THEN need to factor out the RELOAD part, which runs external program to build the image from the fasls.
;;;   ;; OK, have image name be "cvmsrcs.image", and copy stuff into there.

;;; ONCE have recompilation working, natively in the VM,
;;; next step is cross compiling darwinx8664.

;; This is used to cross compile cvm to get initial image.
(defun compile-cvm (&optional force)
  (load "ccl:compiler;cvm;cvm-arch")
  ;; this gets required by loading cvm2.lisp.  Have to compile it so require can find it.
  (compile-file "ccl:compiler;cvm;cvm-backend.lisp" :output-file "ccl:bin;cvm-backend" :verbose t :load t)
  ;; TEMP while debugging. reload stuff we redefined, until build a new lisp with the changes.
  (let ((*warn-if-redefine-kernel* nil))
    ;(load "ccl:lib;systems.lisp") ;; make sure we have the latest, avoid bootstrapping issuess.
    ;(load "ccl:lib;compile-ccl.lisp")
    ;(load "ccl:lib;macros.lisp")
    ;(load "ccl:lib;foreign-types.lisp")
    ;(load "ccl:lib;db-io.lisp")
    ;(load "ccl:library;sockets.lisp")
    ;(load "ccl:lib;nfcomp.lisp")
    ;(load "ccl:lib;compile-ccl.lisp")
    )

  (let* ((*features* *features*)
         (*save-source-locations* NIL)
         (*cerror-on-constant-redefinition* t)
         (*package* (find-package :ccl))
         (*save-doc-strings* t)
         (*fasl-save-doc-strings* t)
         (*aux-modules* (set-difference *aux-modules* *modules-not-for-cvm*))
         (*code-modules* (set-difference *code-modules* *modules-not-for-cvm*))
         (*compiler-modules* (set-difference *compiler-modules* *modules-not-for-cvm*))
         ;; Send all output to cvmsrcs.
         (*ccl-system* (loop for (module fasl . sources) in *ccl-system*
                         collect (list* module (merge-pathnames "ccl:cvmsrcs;" fasl) sources)))
         ;; (cross-compile-ccl t) will reload sysdef-modules (i.e. systems and compile-ccl) as first thing,
         ;; which would override all our careful rebinding above.
         (*aux-modules* (append *sysdef-modules* *aux-modules*))
         (*sysdef-modules* nil))

    ;; Compile level-0
    ;; TODO: Maybe should make a *level-0-files* so don't rely on contents of directories..
    (with-global-optimization-settings ()
      (ensure-directories-exist "ccl:cvmsrcs;level-0;")
      (when force (mapcar #'delete-file (directory (merge-pathnames "ccl:cvmsrcs;level-0;*"
                                                                    (backend-target-fasl-pathname *cvm-backend*)))))
      ;(if force (xload-level-0 :force) (xload-level-0))
      (let* ((level-0-systems
              (loop for dir in '("ccl:level-0;" "ccl:level-0;CVM;")
                nconc (loop for src in (sort (directory (merge-pathnames dir "*.lisp")) #'string< :key #'namestring)
                        collect (list (intern (string-upcase (pathname-name src)) :ccl)
                                      (merge-pathnames "ccl:cvmsrcs;level-0;" src)
                                      src))))
             (*ccl-system* (append level-0-systems *ccl-system*))
             (target (backend-name *cvm-backend*)))

        (with-cross-compilation-target (target)
          (let ((*target-backend* *cvm-backend*))
            (target-compile-modules (mapcar #'car level-0-systems) target force))))


      (ensure-directories-exist "ccl:cvmsrcs;")
      (when force (mapcar #'delete-file (directory (merge-pathnames "ccl:cvmsrcs;*"
                                                                    (backend-target-fasl-pathname *cvm-backend*)))))
      (cross-compile-ccl :darwincvm (not (null force))))))


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
         (bslambda (lfun-bslambda fn)))
    (if print
      (pprint bslambda)
      bslambda)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Compiling files

(require 'faslenv "ccl:xdump;faslenv")

(defvar *ev2-fcomp-hash*)
(defvar *ev2-fcomp-eref*)
(defvar *ev2-bsquote*)

;; Really would be so much easier to intercept this in fcomp-form-1
(defun set-package-call-p (fn)
  (let ((bslambda (lfun-bslambda fn)))
    (destructuring-bind (name argspecs body nlocals) (cdr bslambda)
      (when (and (equal name '($bs-quote nil))
                 (every #'null (butlast argspecs))
                 (zerop nlocals)
                 (eql (length body) 3)
                 (eq (car body) '$bs-funcall)
                 (equal (cadr body) '($bs-quote ccl::set-package))
                 (eq (car (caddr body)) '$bs-quote))
        (cadr (caddr body))))))
        

(defun fasl-dump-cvm-file (toplevel-forms hash output-file)
  ;;(assert (equalp (pathname-type output-file) (pathname-type (bscompile-fasl))))
  (with-open-file (outf output-file :direction :output :if-exists :supersede)
    (format outf "(cl:in-package :ccl-vm)~%($FASL-INIT ~d.)~2%" (hash-table-count hash))
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
                              (check-type (car args) xfunction)
                              (let ((pkg (set-package-call-p (car args))))
                                (if pkg
                                  (progn
                                    (setq args (list pkg))
                                    '$fasl-set-package)
                                  `$fasl-funcall)))
                             ((eq op $fasl-defun)
                              (check-type (car args) xfunction)
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
                  :stream outf :pretty t :readably t :structure nil
                  :right-margin 150)
        and do (terpri outf)))))


(defun ev2-maker-form (obj)
  (let ((info (gethash obj *ev2-fcomp-hash*)))
    (cond ((fixnump info) `($fs-ref ,info))
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
  (cond ((typep obj '(or fixnum single-float standard-char boolean)) (ev2-maybe-store obj store-index))
        ((typep obj 'character) (ev2-maybe-store `($fs-char ,(char-code obj)) store-index))
        ((eq obj (%unbound-marker)) (ev2-maybe-store '($fs-unbound-marker) store-index))
        ((eq obj (%slot-unbound-marker)) (ev2-maybe-store '($fs-slot-unbound-marker) store-index))
        ((eq obj (%illegal-marker)) (ev2-maybe-store '($fs-illegal-marker) store-index))
        ((typep obj 'number) (ev2-number-maker obj store-index))
        ((consp obj) (ev2-cons-maker obj store-index))
        ((symbolp obj) (ev2-symbol-maker obj store-index))
        ((typep obj 'function) (ev2-function-maker obj store-index))
        ((typep obj 'xfunction) (ev2-function-maker obj store-index))
        ((typep obj 'simple-base-string) (ev2-string-maker obj store-index))
        ((typep obj 'simple-vector) (ev2-simple-vector-maker obj store-index))
        ((typep obj '(simple-array * (*))) (ev2-ivector-maker obj store-index))
        ((typep obj 'simple-array) (ev2-array-maker obj store-index))
        ((typep obj 'package) (ev2-package-maker obj store-index))
        ((istructp obj) (ev2-istruct-maker obj store-index))
        ;; It wouldn't be hard to dump arbitrary gvectors/ivectors, but it's not needed.
        (t (error "invalid constant ref ~s" obj))))

(defun ev2-maybe-store (form store-index)
  (if store-index `($fs-set ,store-index ,form) form))

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

;; Could output symbols directly...  
;; Should at least output CL symbols directly
(defun ev2-symbol-maker (sym store-index)
  (let* ((inverse (fasl-setf-name-inverse-p sym)))
    (if inverse
      (progn
        (assert (null store-index)) ;; sym never got scanned so shouldn't have a store-index
        (ev2-maker-form inverse))
      (cond (*ev2-bsquote*
             (ev2-maybe-store
              `($fs-symbol ,(ev2-maker-form (symbol-name sym))
                           ,(ev2-maker-form (symbol-package sym)))
              store-index))
            ((null (symbol-package sym))  ;; gensyms are used as tags in tagbody
             (unless store-index
               (error "An unstored uninterned symbol??? ~s" sym))
             (ev2-maybe-store `(make-symbol ,(symbol-name sym)) store-index))
            (t
             ;; Don't bother storing interned symbols
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
  (let ((bslambda (lfun-bslambda fn)))
    (let ((*ev2-bsquote* nil))
      `($fs-init-function ,(ev2-maybe-store '($fs-cons-function) store-index)
                         ,(ev2-maker-form bslambda)))))

