(in-package :ccl)

;; The CVM file compiler: writes the .bc files.  Loaded in every host, like nfcomp, which calls it.

;; In the VM these are host functions (see cvm-backend).
#-cvm-target
(defun make-bclambda-lfun (bclambda)
  (let ((xfn (%alloc-misc 1 target::subtag-xfunction)))
    (setf (uvref xfn 0) bclambda)
    xfn))

#-cvm-target
(defun lfun-bclambda (xfn)
  (uvref (require-type xfn 'xfunction) 0))

;; Calls FUNCTION on each quoted constant in XFN's bclambda.
#-cvm-target
(defun map-bclambda-immediates (xfn function)
  (labels ((scan (expr)
             (when (consp expr)
               (if (eq (car expr) '$bc-quote)
                 (progn
                   (assert (null (cddr expr)))
                   (funcall function (cadr expr)))
                 (mapc #'scan expr)))))
    (scan (lfun-bclambda xfn))))

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
   (with-standard-io-syntax
     ;; The file is read in the host's CCL-VM package, which stands for CCL, so always print relative to CCL.
    (let ((*package* (find-package "CCL"))
          ;; :readably is passed explicitly when needed, leaving it globally true would
          ;; also affect any debugger entered from this code.
          (*print-readably* nil)
          ;; The VM reads .bc files with *read-eval* nil, so don't let readable printing fall back on #.
          (*read-eval* nil))
    (let* ((*ev2-fcomp-hash* hash)
           (*ev2-fcomp-eref* -1)
           (*ev2-bcquote* nil)
           (maker-forms
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
              collect (cons bs-opcode (mapcar #'(lambda (arg)
                                                  (let ((*ev2-bcquote* t))
                                                    (ev2-maker-form arg)))
                                              args)))))
      ;; The table size is the number of slots the maker forms actually store into, so it's
      ;; known only once they've all been made.
      (format outf "(cl:in-package :ccl-vm)~%($FASL-INIT ~d.)~2%" (1+ *ev2-fcomp-eref*))
      (dolist (maker-form maker-forms)
        (write maker-form :stream outf :pretty t :readably t :structure nil :right-margin 150)
        (terpri outf)))))))


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
           ;; Allocate a slot only for objects that are actually stored, so the slot numbers
           ;; don't depend on how many unstored objects the scan saw more than once.
           (let ((store-index (and (ev2-stored-p obj) (incf *ev2-fcomp-eref*))))
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

;; Whether an object referenced more than once gets a slot in the fasl table.
(defun ev2-stored-p (obj)
  (cond ((typep obj '(or (signed-byte 60) character boolean immediate))  ;; don't store immediates
         nil)
        ((and (symbolp obj) (not *ev2-bcquote*))
         ;; Don't bother storing interned symbols, see ev2-symbol-maker.  Only the bc operators
         ;; should occur more than once in the code.
         ;; if change this, also change make-bclambda-lfun
         (unless (or (eq obj 'bclambda) (string= "$BC-" (string obj) :end2 4))
           (format *trace-output* "~&NOT storing ~s" obj)
           (break "How did this find its way here?"))
         nil)
        (t t)))

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
             (assert (null store-index)) ;; see ev2-stored-p
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

