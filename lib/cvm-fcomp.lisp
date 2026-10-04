(in-package :ccl)

#+cvm-target
(defun xload-level-0 (&optional force)
  (declare (ignore force))
  (cvm-compile-level-0))

(defun cvm-compile-level-0 ()
  ;; TODO: Maybe it's time to make a *level-0-files* so don't rely on contents of directories..
  (assert (eq *target-backend* *cvm-backend*))
  (let* ((*ccl-system*
          (loop for dir in '("ccl:level-0;" "ccl:level-0;CVM;")
            nconc (loop for src in (sort (directory (merge-pathnames dir "*.lisp")) #'string< :key #'namestring)
                    collect (list (intern (string-upcase (pathname-name src)) :ccl)
                                  (make-pathname :type nil :defaults src)
                                  src)))))
    (target-compile-modules (mapcar #'car *ccl-system*) (backend-name *target-backend*) nil)))

#-CVM-TARGET
(defun cross-compile-cvm (&optional force)
  ;; * who does these first few binding for other platforms?  In fact, how does cross compilation happen for other
  ;; platforms -- there are no calls to cross-load-level-0 etc.
  (let* ((*features* *features*)
         (*save-source-locations* NIL)
         (*cerror-on-constant-redefinition* t)
         (*package* (find-package :ccl))
         (*save-doc-strings* t)
         (*fasl-save-doc-strings* t)
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
      (cross-compile-ccl target (not (null force))))

    #+no ;; this is more like rebuild
    (when force
      (collect-all-fasls "ccl:xcvmsrcs;"))))

#-cvm-target ;; this is mostly for testing
(defun cvm-compile (lambda)
  "Compile LAMBDA with the CVM backend, from a ccl running on a real machine."
  (let ((target (backend-name *cvm-backend*)))
    (with-cross-compilation-target (target)
      (let ((*target-backend* *cvm-backend*))
        (compile-named-function lambda :target target)))))


;; Maybe don't even need them, just zip up the whole system, sources and all, and that's what you've got.
;;  The only case would be if we want to check them into a version control system...  Figure that out later.
(defun collect-all-fasls (&optional (dest "ccl:cvmsrcs;"))
  (let ((*.fasl-pathname* (backend-target-fasl-pathname *cvm-backend*)))
    ;; Delete dest before get fasls so that don't try to copy the fasls from dest
    (recursive-delete-directory dest :if-does-not-exist nil)
    (copy-all-fasls "ccl:" dest)))

(defun restore-all-fasls (&optional (src "ccl:cvmsrcs;"))
  (let ((*.fasl-pathname* (backend-target-fasl-pathname *cvm-backend*)))
    (copy-all-fasls src "ccl:" :if-exists :supersede)))

(defun copy-all-fasls (srcdir destdir &key (if-exists :error))
  (let* ((fasls-path (full-pathname (merge-pathnames (merge-pathnames "**/*" srcdir) *.fasl-pathname*)))
         (ndirs (1- (length (pathname-directory fasls-path))))
         (seen-dirs ()))
    (loop for file in (directory fasls-path :follow-links nil)
      as subdirs = (nthcdr ndirs (pathname-directory file))
      as destsubdir = (merge-pathnames (make-pathname :directory `(:relative ,@subdirs)) destdir)
      do (unless (member subdirs seen-dirs :test 'equal)
           (push subdirs seen-dirs)
           (ensure-directories-exist destsubdir))
      do (copy-file file (merge-pathnames destsubdir file) :if-exists if-exists))))

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
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Foreign records, deferred to runtime.
;;;;; The CVM target has no interface database, so it sets the ftd :defer-to-runtime attribute,
;;;;; which then calls into these functions to generate runtime lookups.

(defun %deferred-load-record (name)
  (assert (keywordp name))
  ;; This used to be necessary but at this point it's just an optimization..
  (if (eq name :address) ;; shouldn't be necessary any more?
    (or (info-foreign-type-definition name)
        (progn
          (break "No info for  :address?")
          (make-foreign-pointer-type))) ;; load-record
    (or (info-foreign-type-definition name)
        (make-foreign-record-type :kind :struct :name name))))

(defun %deferred-foreign-access-form (base-form record-name bit-offset accessors)
  (assert (zerop bit-offset)) ;;; ** TODO: if this never triggers, get rid of the arg.
  `(cvm-access-foreign-field ,base-form
                             ',(if accessors (cons record-name accessors) record-name)
                             ,bit-offset))

(defsetf cvm-access-foreign-field setf-cvm-access-foreign-field)

(defun %deferred-foreign-array-access-form (base-form name index-form)
  (assert (keywordp name))
  `(cvm-access-foreign-array ,base-form ',name ,index-form))

(defun %deferred-foreign-size-form (type-name units accessors)
  (let ((form `(cvm-foreign-bit-size '(,type-name ,@accessors))))
    (ecase units
      (:bits form)
      (:bytes `(ash (%i+ ,form 7) -3))
      (:words `(ash (%i+ ,form 31) -5)))))

(defun %deferred-field-offset-form (record-name field-name)
  ;; this is for get-field-offset which returns 3 values, but the last 2 are never used in ccl
  `(values (cvm-foreign-field-byte-offset ',record-name ',field-name) 'unimplemented-record-type 0))

(defun %deferred-foreign-init-forms (ptr record-name inits)
  (when inits
    (assert (keywordp record-name))
    (assert (or (null (cdr inits)) (evenp (length inits))))
    (if (null (cdr inits))
      `((setf ,(%deferred-foreign-access-form ptr record-name 0 ()) ,(car inits)))
      ;; make like %foreign-record-field-forms
      (loop for (key valform) on inits by #'cddr
        do (assert (keywordp key))
        collect `(setf ,(%deferred-foreign-access-form ptr record-name 0 (list key))
                       ,valform)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Compiling files

;; Figure out if we need this at runtime
(eval-when (compile eval)
  (require 'faslenv "ccl:xdump;faslenv"))

(defvar *ev2-fcomp-hash*)
(defvar *ev2-fcomp-eref*)
(defvar *ev2-bcquote*)

;; Really would be so much easier to intercept this in fcomp-form-1
(defun set-package-call-p (fn)
  (let ((bclambda (lfun-bclambda fn)))
    (destructuring-bind (name argspecs body nlocals) (cdr bclambda)
      (when (and (equal name '($bc-quote nil))
                 (every #'null (butlast argspecs))
                 (zerop nlocals)
                 (eql (length body) 3)
                 (eq (car body) '$bc-funcall)
                 (equal (cadr body) '($bc-quote ccl::set-package))
                 (eq (car (caddr body)) '$bc-quote))
        (cadr (caddr body))))))
        

(defun fasl-dump-cvm-file (toplevel-forms hash output-file)
  ;;(assert (equalp (pathname-type output-file) (pathname-type (bscompile-fasl))))
  (with-open-file (outf output-file :direction :output :if-exists :supersede)
    ;; Potentially might have separate slots for VM and CL versions of the objects, hence 2x
    (format outf "(cl:in-package :ccl-vm)~%($FASL-INIT ~d.)~2%" (* 2 (hash-table-count hash)))
    (let* ((*ev2-fcomp-hash* hash)
           (*ev2-fcomp-eref* -1)
           (*ev2-bcquote* nil))
      (loop for form in toplevel-forms as (op . args) = form
        as bs-opcode = (cond ((eq op $fasl-platform) nil)
                             ((eq op $fasl-src) nil #+NOT-YET '$fasl-record-source)
                             ((eq op $fasl-toplevel-location) 
                              (check-type (car args) (or source-note null))
                              nil #+NOT-YET '$fasl-toplevel-location nil)
                             ((eq op $fasl-lfuncall)
                              (check-type (car args) (or xfunction function))
                              (let ((pkg (set-package-call-p (car args))))
                                (if pkg
                                  (progn
                                    (setq args (list pkg))
                                    '$fasl-set-package)
                                  `$fasl-funcall)))
                             ((eq op $fasl-defun)
                              (check-type (car args) (or xfunction function))
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
        ;; Ugh, wait, if all of them will be $BC-QUOTE, why do we bother???
        do (write (cons bs-opcode (mapcar #'(lambda (arg)
                                              (let ((*ev2-bcquote* t))
                                                (ev2-maker-form arg)))
                                          args))
                  :stream outf :pretty t :readably t :structure nil
                  :right-margin 150)
        and do (terpri outf)))))


(defun ev2-fcomp-info (obj)
  ;; Need to track quoted(VM) and unquoted(CL) objects separately
  ;; Info #(<quoted-info> <unquoted-info>), or just <info> which is like #(<info> <info>)
  (let ((info (gethash obj *ev2-fcomp-hash*)))
    (if (vectorp info)
      (svref info (if *ev2-bcquote* 0 1))
      info)))

(defun (setf ev2-fcomp-info) (new-info obj)
  (let ((info (gethash obj *ev2-fcomp-hash*)))
    (unless (eq new-info info)
      (unless (vectorp info)
        (setf (gethash obj *ev2-fcomp-hash*) (setq info (vector info info))))
      (setf (svref info (if *ev2-bcquote* 0 1)) new-info)))
  new-info)

(defun ev2-maker-form (obj)
  (let* ((info (ev2-fcomp-info obj)))
    (cond ((fixnump info) `($fs-ref ,info))
          ((eq info t)
           (let ((store-index (incf *ev2-fcomp-eref*)))
             (when (typep obj '(or (signed-byte 60) character boolean immediate))  ;; don't store immediates
               (setq store-index nil))
             (setf (ev2-fcomp-info obj) store-index)
             (ev2-maker-dispatch obj store-index)))
          ((null info)
           (ev2-maker-dispatch obj nil))
          (t
           (destructuring-bind (load-form scanned-p referenced-p compiled-initform) info
             (declare (ignore scanned-p))
             ;;(assert scanned-p)
             (let ((maker (ev2-maker-form load-form)))
               ;; Referenced-p NIL means this FORM is referenced only once, so won't need to store it.
               ;; Referenced-p T means it got referenced more than once.  In this case, load-form must have gotten stored.
               (when referenced-p
                 (setf (ev2-fcomp-info obj) (require-type (ev2-fcomp-info load-form) 'fixnum)))
               (if compiled-initform
                 `(prog1 ,maker ,(ev2-maker-form compiled-initform))
                 maker)))))))

(defun ev2-maker-dispatch (obj store-index)
  (cond ((typep obj '(or fixnum single-float standard-char boolean)) (ev2-maybe-store obj store-index))
        ((typep obj 'character) (ev2-maybe-store `($fs-char ,(char-code obj)) store-index))
        ((typep obj 'immediate) (ev2-immediate-maker obj store-index))
        ((typep obj 'number) (ev2-number-maker obj store-index))
        ((consp obj) (ev2-cons-maker obj store-index))
        ((symbolp obj) (ev2-symbol-maker obj store-index))
        ((typep obj '(or xfunction function)) (ev2-function-maker obj store-index))
        ((typep obj 'simple-base-string) (ev2-string-maker obj store-index))
        ((typep obj 'simple-vector) (ev2-simple-vector-maker obj store-index))
        ((typep obj '(simple-array * (*))) (ev2-ivector-maker obj store-index))
        ((typep obj 'simple-array) (ev2-array-maker obj store-index))
        ((typep obj 'package) (ev2-package-maker obj store-index))
        ((istructp obj) (ev2-istruct-maker obj store-index))
        ;; It wouldn't be hard to dump arbitrary gvectors/ivectors, but it's not needed.
        (t (error "invalid constant ref ~s" obj))))

(defun ev2-immediate-maker (obj store-index)
  (assert *ev2-bcquote*)
  (cond ((eq obj (%unbound-marker)) (ev2-maybe-store '($fs-unbound-marker) store-index))
        ((eq obj (%slot-unbound-marker)) (ev2-maybe-store '($fs-slot-unbound-marker) store-index))
        ((eq obj (%illegal-marker)) (ev2-maybe-store '($fs-illegal-marker) store-index))
        ((eq obj %unbound-function%) (ev2-maybe-store '($fs-unbound-function) store-index))
        (t (error "unknown immediate ~s" obj))))

(defun ev2-maybe-store (form store-index)
  (if store-index `($fs-set ,store-index ,form) form))

(defun ev2-string-maker (string store-index)
  (check-type string simple-string)
  (ev2-maybe-store (if *ev2-bcquote* `($fs-string ,string) string) store-index))

(defun ev2-number-maker (number store-index)
  (if *ev2-bcquote*
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
  (assert *ev2-bcquote*)
  (ev2-maybe-store `($fs-package ,(ev2-maker-form (package-name pkg))) store-index))

;; Could output symbols directly...  
;; Should at least output CL symbols directly
(defun ev2-symbol-maker (sym store-index)
  (let* ((inverse (fasl-setf-name-inverse-p sym)))
    (if inverse
      (progn
        (assert (null store-index)) ;; sym never got scanned so shouldn't have a store-index
        (ev2-maker-form inverse))
      (cond (*ev2-bcquote*
             (ev2-maybe-store
              `($fs-symbol ,(ev2-maker-form (symbol-name sym))
                           ,(ev2-maker-form (symbol-package sym)))
              store-index))
            ((null (symbol-package sym))  ;; gensyms are used as tags in tagbody
             (error "Who's using gensyms?") ;; not anymore.
             (unless store-index
               (error "An unstored uninterned symbol??? ~s" sym))
             (ev2-maybe-store `(make-symbol ,(symbol-name sym)) store-index))
            (t
             ;; Don't bother storing interned symbols
             (assert (or (eq (symbol-package sym) (symbol-package '$bc-quote))
                         (eq (symbol-package sym) *keyword-package*)))
             (when store-index
               ;; if change this, also change make-bclambda-lfun
               (unless (or (eq sym 'bclambda) (string= "$BC-" (string sym) :end2 4))
                 (format *trace-output* "~&NOT storing ~s" sym)
                 (break "How did this find its way here?"))
               (setf (ev2-fcomp-info sym) nil))
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
  (assert *ev2-bcquote*)
  `($fs-init-uvector ,(ev2-maybe-store
                       `($fs-make-uvector ,type-key ,(uvsize uvec))
                       store-index)
                     ,@(loop for i from 0 below (uvsize uvec)
                         collect (ev2-maker-form (uvref uvec i)))))

(defun ev2-simple-vector-maker (vector store-index)
  (check-type vector simple-vector)
  (cond (*ev2-bcquote*
         (ev2-uvector-maker :simple-vector vector store-index))
        (t
         (assert (not store-index)) ;; could support but not needed
         `(vector ,@(map 'list #'ev2-maker-form vector)))))

(defun ev2-ivector-maker (vector store-index)
  (assert *ev2-bcquote*)
  (check-type vector ivector)
  (ev2-uvector-maker (ev2-element-type-keyword vector) vector store-index))

;; It's not unusual to have 2-dim array immediates...  But maybe not in CCL sources?
(defun ev2-array-maker (arr store-index)
  (assert *ev2-bcquote*)
  (check-type arr simple-array)
  (let ((type (array-element-type arr)))
    (assert (or (eq type t) (subtypep type '(or number character)))))
  (let* ((type-key (ev2-element-type-keyword arr))
         (dims (array-dimensions arr)))
    `($fs-init-array ,(ev2-maybe-store `($fs-make-array ,type-key ',dims) store-index)
                     ,@(loop for i from 0 below (array-total-size arr) 
                         collect (ev2-maker-form (row-major-aref arr i))))))

(defun ev2-istruct-maker (istruct store-index)
  (assert *ev2-bcquote*)
  ;; Assume istruct layout is the same everywhere.
  (ev2-uvector-maker :istruct istruct store-index))

(defun ev2-cons-maker (cons store-index)
  (cond ((eq (car cons) cfasl-load-time-eval-sym)
         (assert *ev2-bcquote*)
         (destructuring-bind (form) (cdr cons)
           (ev2-maybe-store
            (if (funcall-lfun-p form)
              `($fs-funcall ,(ev2-maker-form (cadr form)))
              `($fs-eval ,(ev2-maker-form form)))
            store-index)))
        ((istruct-cell-p cons)
         (assert *ev2-bcquote*)
         (check-type (car cons) symbol)
         (ev2-maybe-store `($fs-istruct-cell ,(ev2-maker-form (car cons))) store-index))
        ((and (not *ev2-bcquote*) (eq (car cons) '$BC-QUOTE))
         (assert (and (cdr cons) (not (cddr cons))))
         (assert (not (ev2-fcomp-info (cdr cons))))
         (let ((val-maker (let ((*ev2-bcquote* t))
                            (ev2-maker-form (cadr cons)))))
           (if store-index
             `(rplacd ,(ev2-maybe-store '(list '$BC-QUOTE) store-index) (list ,val-maker))
             `(list '$BC-QUOTE ,val-maker))))
        (store-index
         `(rplacd (rplaca ,(ev2-maybe-store '(cons nil nil) store-index)
                          ,(ev2-maker-form (car cons)))
                  ,(ev2-maker-form (cdr cons))))
        (t (let* ((rest cons)
                  (val-forms (loop collect (ev2-maker-form (pop rest))
                               while (and (consp rest) (not (ev2-fcomp-info rest))))))
             (if (null rest)
               `(list ,@val-forms)
               `(list* ,@val-forms ,(ev2-maker-form rest)))))))

(defun ev2-function-maker (fn store-index)
  (assert *ev2-bcquote*)
  (let ((bclambda (lfun-bclambda fn)))
    (assert (consp bclambda))
    (let ((*ev2-bcquote* nil))
      `($fs-init-function ,(ev2-maybe-store '($fs-cons-function) store-index)
                         ,(ev2-maker-form bclambda)))))

