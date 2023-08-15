(in-package :ccl)

;; CVM backend for the compiler.  Generates "byte code" ($BC) expressions that can be evaluated by a VM.

(eval-when (:compile-toplevel :execute)
  (require "NXENV")
  ;(require "CVMENV")
  )

(eval-when (:load-toplevel :execute :compile-toplevel)
  (require "CVM-BACKEND"))

;; TODO:  pass the "vreg" arg in, it says whether it's being evaluated for value.

(defvar *cvm2-cur-afunc*)
(defvar *cvm2-lex-vars*)
(defvar *cvm2-tags*)

(defun cvm2-lex-var-p (var)
  (check-type var var)
  (or (cvm2-var-inherited-from var)
      (not (logbitp $vbitspecial (nx-var-bits var)))))

(defun cvm2-var-inherited-from (v)
  (unless (fixnump (var-bits v)) (var-bits v)))

(defun cvm2-quote (obj)
  `($bc-quote ,obj))

(defun assign-misc-vcell (thing)
  (assert (or (consp thing) (vectorp thing))) ;; block is cons, tagbody is vector, temp vars are cons too.
  ;; I think this is ok, if have multiple blocks
  (assign-lex-vcell thing t))


(defun assign-lex-vcell (var &optional misc-p)  ;; var can also be a block tag
  (unless misc-p (assert (cvm2-lex-var-p var)))
  (when (assoc var *cvm2-lex-vars*) (error "~s already assigned" var))
  (let ((index (length *cvm2-lex-vars*)))
    (push (cons var index) *cvm2-lex-vars*)
    index))

(defun get-lex-vcell (var &optional misc-p)
  (unless misc-p (assert (cvm2-lex-var-p var)))
  (cdr (or (assoc var  *cvm2-lex-vars*)
           (error "Unknown lex var ~s" var))))

(defun get-misc-vcell (tag)
  (assert (or (consp tag) (vectorp tag)))
  (get-lex-vcell tag t))

#-cvm-target
(defun make-bclambda-lfun (bclambda)
  (let ((xfn (%alloc-misc 1 target::subtag-xfunction)))
    (setf (uvref xfn 0) bclambda)
    xfn))

#-cvm-target
(defun lfun-bclambda (xfn)
  (uvref (require-type xfn 'xfunction) 0))

(defmacro bclambda-bits (bclambda) `(car (last (third ,bclambda))))

;;; *** TODO: rename to cvm2-
(defun cvm2-compile (afunc &optional lambdaform record-symbols)  ;;x862-compile
  (dolist (a (afunc-inner-functions afunc))
    (unless (afunc-lfun a)
      (assert (eq (afunc-parent a) afunc))
      (cvm2-compile a (if lambdaform (afunc-lambdaform a)) record-symbols)))
  ;;(pprint (decomp-acode (afunc-acode afunc)))
  (let* ((inherited-vars (afunc-inherited-vars afunc))
         (fbits (afunc-bits afunc))
         (bclambda 
          (let ((*cvm2-cur-afunc* afunc)
                (acode (afunc-acode afunc)))
            (assert (eq (acode-operator-sym acode) 'lambda-list))
            (when inherited-vars
              (assert (afunc-parent afunc))
              (assert (let* ((outer-afunc (afunc-parent afunc))
                             (outer-vars (afunc-all-vars outer-afunc))
                             (outer-inh (afunc-inherited-vars outer-afunc)))
                        (loop for v in inherited-vars
                          always (let ((outer-var (cvm2-var-inherited-from v)))
                                   (and outer-var
                                        (or (member outer-var outer-vars)
                                            (member outer-var outer-inh))))))))
            (apply #'cvm2-lambda-form (afunc-name afunc) fbits inherited-vars (acode-operands acode)))))

    (when (and (logbitp $fbitnextmethargsp fbits)
               (not (logbitp $fbitmethodp fbits)))
      (break "When does this happen?")
      (let ((parent (afunc-parent afunc)))
        (when parent
          (bitsetf $fbitnextmethargsp (afunc-bits parent)))))

    (setf (afunc-lfun afunc) (make-bclambda-lfun bclambda))

    ;; now that we have an lfun, fixup any forward refs to the fn.
    (loop for ref in (afunc-fwd-refs afunc)
      do (assert (equal ref `($bc-quote ,afunc)))
      do (setf (cadr ref) (afunc-lfun afunc))))
  afunc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; cvm2-specials


;; This will become a vector like *x862-specials*, but for now.
(defvar *cvm2-specials* (make-hash-table :test 'eq))

(defun acode-operator-sym (x)
  (acode-operator-name (if (acode-p x) (acode-operator x) x)))

(defun cvm2-operator-function (acode)
  ;(svref *cvm2-specials* (%ilogand #.operator-id-mask (acode-operator acode)))
  (or (gethash (acode-operator-sym acode) *cvm2-specials*)
      (progn
        (error "Unknown operator ~s ~s" (acode-operator-sym acode)
               (acode-operands acode)))))

(defun cvm2-form (acode)
  (if (nx-null acode)
    (cvm2-quote nil)
    (if (nx-t acode)
      (cvm2-quote t)
      ;; (and (null vreg) (%ilogbitp operator-acode-subforms-bit op) (%ilogbitp operator-assignment-free-bit op) (%ilogbitp operator-side-effect-free-bit op))
      ;; if the form is assignment free and side effect free, and not being evaluated for value, then can just eval the arguments.
      ;; (dolist (arg (acode-operators form)) (x862-form arg))
      (apply (cvm2-operator-function acode) (acode-operands acode)))))

(defun cvm2-arglist-forms (arglist)
  (destructuring-bind (stack-args revreg-args) arglist
    (append stack-args (reverse revreg-args))))

(defmacro defcvm2 (operator-name-or-names arglist &body forms)
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
               nconc (list `(setf (gethash ',operator-name *cvm2-specials*) fn)))
             (list `(setf (gethash ',operator-name-or-names *cvm2-specials*) fn)))))))

(defmacro defcvm2-fn (operator arglist runtime-op)
  (assert (every (lambda (x) (and (symbolp x) (not (eql #\& (char (string x) 0))))) arglist))
  (check-type runtime-op symbol)
  (let ((cvm2-args (mapcar (lambda (arg) `(cvm2-form ,arg)) arglist)))
    `(defcvm2 ,operator ,arglist
       (list ',runtime-op ,@cvm2-args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; binding things

(defun cvm2-binding-var (var)
;;  (assert (fixnump (nx-var-bits (require-type var 'var)))) ;; not inherited
  (if (cvm2-lex-var-p var)
    (assign-lex-vcell var)
    (var-name var)))


(defun cvm2-lambda-form (name fbits inh req opt rest keys auxen acode p2decls &aux lexpr (bits 0))
  (declare (ignorable p2decls)) ;; stuff like tail-call-allow, safety, trust-declarations.
  (assert (every #'cvm2-var-inherited-from inh))
  (assert (not (and (consp (car req)) (eq (caar req) '&lap))))
  (when (consp rest)
    (assert (and (null opt) (null keys) (equal auxen '(nil nil))))
    (setq lexpr t rest (car rest))
    (assert (not (logbitp $vbitspecial (nx-var-bits rest)))))
  (when auxen
    (destructuring-bind (vars vals) auxen
      (assert (= (length vars) (length vals)))
      (when vars
        (setq acode (make-acode (%nx1-operator let*) vars vals acode p2decls)))))

  ;; No, leave a slot so can set later.
  ;;(when (null name) (bitsetf $lfbits-noname-bit bits))

  (when (logbitp $fbitnonnullenv fbits) (bitsetf $lfbits-nonnullenv-bit bits))
  (when (logbitp $fbitccoverage fbits) (bitsetf $lfbits-code-coverage-bit bits))
  (when (logbitp $fbitmethodp fbits) (bitsetf $lfbits-method-bit bits))
  (when (logbitp $fbitnextmethp fbits)
    (assert (logbitp $fbitmethodp fbits))
    (bitsetf $lfbits-nextmeth-bit bits))
  (when (logbitp $fbitnextmethargsp fbits)
    (if (logbitp $fbitmethodp fbits)
      (bitsetf $lfbits-nextmeth-with-args-bit bits)
      (break "When does this happen")))
  ;; IS THIS RIGHT?  ONly take an extra arg if nextmethp is true.
  ;; COMPUTE-SLOTS is the first fn compiled that has nextmethp
  (when (and (logbitp $fbitmethodp fbits)
             (not (logbitp $fbitnextmethp fbits)))
    (pop req))

  (let* ((*cvm2-lex-vars* nil)
         (*cvm2-tags* 0)
         (argspecs (list
                    ;; inh
                    (let ((num-inh (length inh)))
                      (setf (ldb $lfbits-numinh bits) (min num-inh (ldb $lfbits-numinh -1)))
                      (mapcar #'cvm2-binding-var inh))
                    ;; req
                    (let ((num-req (length req)))
                      (when (and (logbitp $fbitmethodp fbits) ;; lie, to match x86...
                                 (logbitp $fbitnextmethp fbits)) ;; lie about hte extra arg.
                        (assert (> num-req 0))
                        (decf num-req))
                      (setf (ldb $lfbits-numreq bits) (min num-req (ldb $lfbits-numreq -1)))
                      (mapcar #'cvm2-binding-var req))
                    ;; opt
                    (when opt
                      (destructuring-bind (opt-vars opt-inits opt-supp-vars) opt
                        (let ((num-opt (length opt-vars)))
                          (assert (= num-opt (length opt-inits) (length opt-supp-vars)))
                          (setf (ldb $lfbits-numopt bits) (min num-opt (ldb $lfbits-numopt -1)))
                          (mapcar (lambda (var init supp)
                                    (unless (and (nx-null init) (not supp))
                                      (bitsetf $lfbits-optinit-bit bits))
                                    (list (cvm2-binding-var var)
                                          (cvm2-form init)
                                          (and supp (cvm2-binding-var supp))))
                                  opt-vars opt-inits opt-supp-vars))))
                    ;; rest
                    (when rest
                      (bitsetf (if lexpr $lfbits-restv-bit $lfbits-rest-bit) bits)
                      (cvm2-binding-var rest))
                    ;; keys
                    (when keys
                      (bitsetf $lfbits-keys-bit bits)
                      (destructuring-bind (allow-other-keys-p keyvars keysupp keyinits keykeys) keys
                        (assert (= (length keyvars) (length keysupp) (length keyinits) (length keykeys)))
                        (when allow-other-keys-p (bitsetf $lfbits-aok-bit bits))
                        (cons allow-other-keys-p
                              (map 'list (lambda (key var init supp)
                                           (list (cvm2-quote key) ;; Need to quote it so gets converted
                                                 (cvm2-binding-var var)
                                                 (cvm2-form init)
                                                 (and supp (cvm2-binding-var supp))))
                                   keykeys keyvars keyinits keysupp))))
                    ;; flags
                    bits))
         (body (cvm2-form acode)))
    (when lexpr
      (let ((rest-var (get-lex-vcell rest)))
        (setq body (cvm2-progn
                    `(($bc-lset ,rest-var ($bc-lexpr-args ,rest-var))
                      ,body)))))
    (assert (every #'(lambda (v) (or (member (car v) inh)
                                     (member (car v) (afunc-all-vars *cvm2-cur-afunc*))
                                     (consp (car v)))) ;; block tag
                   *cvm2-lex-vars*))
    `(bclambda ,(cvm2-quote name)
               ,argspecs
               ,body
               ,(length *cvm2-lex-vars*))))


;; Not clear why pass1 doesn't just handle this.
(defcvm2 lambda-bind (vals req rest keys-p auxen body p2decls)
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
  (cvm2-bind nil req vals body p2decls))

(defcvm2 let* (vars vals body p2decls) ;; x862-let*
  (cvm2-bind t vars vals body p2decls))

(defcvm2 let (vars vals body p2decls) ;; x862-let
  (cvm2-bind nil vars vals body p2decls))

(defcvm2 flet (vars afuncs body p2decls)
  (cvm2-bind t vars (mapcar #'nx1-afunc-ref afuncs) body p2decls))

(defcvm2 labels (vars afuncs body p2decls)
  (cvm2-bind nil vars (mapcar #'nx1-afunc-ref afuncs) body p2decls))

(defun cvm2-bind (seq? vars vals body p2decls)
  (declare (ignore p2decls))
  (assert (eql (length vars) (length vals)))
  (let* ((bindings (loop for var in vars for val in vals
                     as bv = (cvm2-binding-var var)
                     as init-form = (cvm2-form val)
                     collect (list (if (and (logbitp $vbitdynamicextent (nx-var-bits var))
                                            ;;mostly don't bother, except we don't want to be consing
                                            ;; gc'able macptrs
                                            (eq (car init-form) '$bc-new-macptr))
                                     (list bv)
                                     bv)
                                   init-form)))
         (body-form (cvm2-form body)))
    (cond ((null bindings) body-form)
          ((assoc '*interrupt-level* bindings)
           (assert (eql (length bindings) 1))
           `($bc-with-interrupt-level ,(cadr (car bindings)) ,body-form))
          ;; If there are no special variables, can treat a let as let*
          ((or seq? (loop for b in bindings never (symbolp (car b))))
           (when (eq (car body-form) '$bc-let*)
             (destructuring-bind (inner-bindings inner-form) (cdr body-form)
               (setq bindings (append bindings inner-bindings))
               (setq body-form inner-form)))
           (labels ((ssplit (bindings body-form)
                      (if (null bindings)
                        body-form
                        (let ((lex-bindings (loop while (and bindings (fixnump (car (car bindings))))
                                              collect (pop bindings))))
                          (if (null bindings)
                            `($bc-let* ,lex-bindings ,body-form)
                            (let* ((v (pop bindings))
                                   (body-form
                                    (if (consp (car v))
                                      `($bc-stack-block ,(caar v) ,@(cdr (cadr v))
                                                        ,(ssplit bindings body-form))
                                      `($bc-progv ,(cvm2-quote (list (car v))) ($bc-list ,(cadr v))
                                                  ,(ssplit bindings body-form)))))
                              (if (null lex-bindings)
                                body-form
                                `($bc-let* ,lex-bindings ,body-form))))))))
             (ssplit bindings body-form)))
          (t
           (let ((special-vars ())
                 (special-vals ()))
             (loop for b in bindings
               ;; mixing specials and stack block too hard...  Bet it never happens!
               do (assert (not (consp (car b))))
               unless (fixnump (car b)) do (let* ((temp (assign-misc-vcell b)))
                                             (push (car b) special-vars)
                                             (push `($bc-lref ,temp) special-vals)
                                             (setf (car b) temp)))
             (assert special-vars)
             `($bc-let* ,bindings
                        ($bc-progv ,(cvm2-quote special-vars) ($bc-list ,@special-vals) ,body-form)))))))

(defcvm2 multiple-value-bind (vars val body p2decls)  ;x862-multiple-value-bind
  (declare (ignore p2decls))
  (assert (cdr vars)) ;; just to see if there's any reason to try to optimize this.
  `($bc-multiple-value-bind ,(mapcar #'cvm2-binding-var vars) ,(cvm2-form val) ,(cvm2-form body)))

(defcvm2 multiple-value-prog1 (exprs)
  (assert exprs)
  (let ((valform (pop exprs)))
    (if exprs
      `($bc-multiple-value-prog1 ,(cvm2-form valform)
                                ,(cvm2-progn (mapcar #'cvm2-form exprs)))
      (cvm2-form valform))))

(defcvm2-fn progv (symbols values body) $bc-progv)

(defcvm2-fn multiple-value-list (form) $bc-multiple-value-list)

(defcvm2-fn nth-value (n form) $bc-nth-value)

(defcvm2 values (forms)
  `($bc-values ,@(mapcar #'cvm2-form forms)))

(defcvm2-fn unwind-protect (protected-form cleanup-form) $bc-unwind-protect)

(defcvm2 lexical-reference (var)
  `($bc-lref ,(get-lex-vcell var)))

(defcvm2 setq-lexical (var value)
  `($bc-lset ,(get-lex-vcell var) ,(cvm2-form value)))

(defcvm2 inherited-arg (arg) ;; x862-inherited-arg
  `($bc-vcell-ref ,(get-lex-vcell arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;;;; calls, values

(defun cvm2-progn (forms)
  (if (null forms)
    (cvm2-quote nil)
    (let ((forms (loop for form in forms
                   when (eq (car form) '$bc-progn)
                   append (cdr form)
                   else collect form)))
      (if (cdr forms)
        `($bc-progn ,@forms)
        (car forms)))))


(defcvm2 progn (exprs) ;; x862-progn
  (cvm2-progn (mapcar #'cvm2-form exprs)))

;; Ugh, why doesn't pass1 just macroexpand it?  It's harder now because can't make a variable.
(defcvm2 prog1 (exprs)
  (assert exprs)
  `($bc-prog1 ,@(mapcar #'cvm2-form exprs)))

(defcvm2 %decls-body (body p2decls)
  (declare (ignore p2decls))
  (cvm2-form body))

(defun cvm2-augmented-arglist (afunc arglist) ;; augmented with inherited variables
  (append (loop with outer = (afunc-inherited-vars *cvm2-cur-afunc*)
            for var in (afunc-inherited-vars afunc)
            as root-var = (nx-root-var var)
            collect (make-acode (%nx1-operator inherited-arg)
                                (or (find root-var outer :key #'nx-root-var) root-var)))
          (cvm2-arglist-forms arglist)))

(defcvm2 call (fn arglist &optional spread-p) ;; x862-call
  (assert (acode-p fn))
  `(,(if spread-p '$bc-apply '$bc-funcall)
    ,(cvm2-form fn)
    ,@(mapcar #'cvm2-form (cvm2-arglist-forms arglist))))

(defcvm2 lexical-function-call (afunc arglist &optional spread-p)
  `(,(if spread-p '$bc-apply '$bc-funcall)
    ,(afunc-lfun-ref afunc)
    ,@(mapcar #'cvm2-form (cvm2-augmented-arglist afunc arglist))))

(defcvm2 self-call (arglist &optional spread-p)
  ;; Call back to the function being compiled.  %double-float does this.
  ;; also compile=named-function
  `(,(if spread-p '$bc-apply '$bc-funcall)
    ($bc-this-function) 
    ,@(mapcar #'cvm2-form (cvm2-augmented-arglist *cvm2-cur-afunc* arglist))))

(defcvm2 multiple-value-call (fn-form arglist)
  `($bc-mvcall ,(cvm2-form fn-form) ,@(mapcar #'cvm2-form arglist)))

(defcvm2 typed-form (type form &optional check-p)
  (when (equal type #+64-bit-target *nx-64-bit-fixnum-type* #+32-bit-target *nx-32-bit-fixnum-type*)
    (setq type 'fixnum))
  (if check-p
    `(,(or (cdr (assoc type '((fixnum . $bc-require-fixnum)
                              (cons . $bc-require-cons)
                              (list . $bc-require-list)
                              (symbol . $bc-require-symbol)
                              (integer . $bc-require-integer)
                              (gvector . $bc-require-gvector)
                              (number . $bc-require-number)
                              (real . $bc-require-real)
                              (character . $bc-require-character)
                              (simple-string . $bc-require-simple-string)
                              (simple-vector . $bc-require-simple-vector)
                              ((signed-byte 8) . $bc-require-s8)
                              ((unsigned-byte 8) . $bc-require-u8)
                              ((signed-byte 16) . $bc-require-s16)
                              ((unsigned-byte 16) . $bc-require-u16)
                              ((signed-byte 32) . $bc-require-s32)
                              ((unsigned-byte 32) . $bc-require-u32)
                              ((signed-byte 64) . $bc-require-s64)
                              ((unsigned-byte 64) . $bc-require-u64))
                       :test 'equal))
           (error "unsupported type ~s" type))
      ,(cvm2-form form))
    (cvm2-form form)))

;;;  **** TODO: now that we're not trying to bootstrap from nothing, this could all just be require-type

(defcvm2-fn require-fixnum (obj) $bc-require-fixnum)
(defcvm2-fn require-integer (obj) $bc-require-integer)
(defcvm2-fn require-number (obj) $bc-require-number)
(defcvm2-fn require-real (obj) $bc-require-real)
(defcvm2-fn require-character (obj) $bc-require-character)
(defcvm2-fn require-list (obj) $bc-require-list)
(defcvm2-fn require-symbol (obj) $bc-require-symbol)
(defcvm2-fn require-simple-string (obj) $bc-require-simple-string)
(defcvm2-fn require-simple-vector (obj) $bc-require-simple-vector)
(defcvm2-fn require-s8 (obj) $bc-require-s8)
(defcvm2-fn require-u8 (obj) $bc-require-u8)
(defcvm2-fn require-s16 (obj) $bc-require-s16)
(defcvm2-fn require-u16 (obj) $bc-require-u16)
(defcvm2-fn require-s32 (obj) $bc-require-s32)
(defcvm2-fn require-u32 (obj) $bc-require-u32)
(defcvm2-fn require-s64 (obj) $bc-require-s64)
(defcvm2-fn require-u64 (obj) $bc-require-u64)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; immediates
#+GZ
(defmethod print-object ((v var) stream)
  (print-unreadable-object (v stream :type t :identity t)
    (format stream "~s" (var-name v))
    (unless (fixnump (var-bits v))
      (format stream " inh ~s" (var-bits  v)))))

;; x862-fixnum 
(defcvm2 fixnum (value) (cvm2-quote value))

(defcvm2 immediate (value) ;; x862-immediate
  ;; Don't really need to do anything special for loadtime value, it's communicated from
  ;; compiler pass1 to file-compiler.
  ;;(if (and (listp value) *load-time-eval-token* (eq (car value) *load-time-eval-token*)) ..)
  (cvm2-quote value))

(defcvm2 simple-function (afunc) ;; x862-simple-function just does immediate for x862-afunc-lfun-ref
  (afunc-lfun-ref afunc))

(defun afunc-lfun-ref (afunc)
  (if (afunc-lfun afunc)
    (cvm2-quote (afunc-lfun afunc))
    ;; This first happens in nx-record-code-coverage-acode
    (let ((ref (copy-list (cvm2-quote afunc))))
      (push ref (afunc-fwd-refs afunc))
      ref)))


;; At runtime, this will create a function that does (apply inner (vcell 1) (vcell 2)  ... ARGS),
;; We can't do it here because can't create a new variable!!
;;;; * OR  MAYBE WE CAN?  can  bind misc.
(defcvm2 closed-function (inner-afunc)
  `($bc-closed-function ,(afunc-lfun-ref inner-afunc)
                        ,(loop for inner-var in (afunc-inherited-vars inner-afunc)
                           as var = (cvm2-var-inherited-from inner-var)
                           do (assert (or (member var (afunc-all-vars *cvm2-cur-afunc*))
                                          (member var (afunc-inherited-vars *cvm2-cur-afunc*))))
                           collect (get-lex-vcell var))))

;; (test-fn '(lambda (a b) (list #'(LAMBDA (x) (+ x b)) a)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; global vars/fns

(defcvm2 (special-ref global-ref free-reference bound-special-ref) (sym) ;; x862-special-ref
  (if (eq sym '*interrupt-level*)
    '($bc-interrupt-level)
    `($bc-symbol-value ($bc-quote ,sym))))

;;; *** RENAME TO $bc-SET-SYMBOL-VALUE
(defcvm2 (setq-special setq-free global-setq) (sym val)
  (check-type sym symbol)
  `($bc-setq-special ($bc-quote ,sym)  ,(cvm2-form val)))

(defcvm2 %function (sym)
  (check-type sym symbol)
  `($bc-symbol-function ($bc-quote ,sym)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; test, compare

(defun cvm2-cc-operand (acode)
  (acode-immediate-operand acode))

;; Ops that return T or NIL.
(defun cvm2-boolean-op-p (op)
  (member op '($bc-not $bc-yes $bc-eq $bc-gt $bc-le $bc-lt $bc-ge $bc-characterp $bc-endp
                      $bc-logbitp $bc-istruct-typep $bc-base-char-p $bc-macptr-eql)))

(defun cvm2-boolean-form (cc op &rest args)
  (ecase (cvm2-cc-operand cc)
    (:EQ `(,op ,@args))
    (:NE (if (eq op '$bc-not)
           (let ((arg (car args)))
             (if (cvm2-boolean-op-p (car arg))
               arg
               `($bc-yes ,arg)))
           `($bc-not (,op ,@args))))))


(defcvm2 not (cc val) ;;x862-not
  (cvm2-boolean-form cc '$bc-not (cvm2-form val)))

;; lots of constant folding opportunities here see x862-eq-test
(defcvm2 (eq neq) (cc form1 form2)
  (let ((form `($bc-eq ,(cvm2-form form1) ,(cvm2-form form2))))
    (ecase (cvm2-cc-operand cc)
      (:EQ form)
      (:NE `($bc-not ,form)))))

(defcvm2 (numcmp short-float-compare double-float-compare %i<> %natural<>) (cc form1 form2) ;;x862-numcmp
  (let ((forms (list (cvm2-form form1) (cvm2-form form2))))
    (ecase (cvm2-cc-operand cc)
      (:EQ `($bc-= ,@forms))
      (:NE `($bc-not ($bc-= ,@forms)))
      (:GT `($bc-gt ,@forms))
      (:LE `($bc-not ($bc-gt ,@forms)))
      (:LT `($bc-lt ,@forms))
      (:GE `($bc-not ($bc-lt ,@forms))))))

(defcvm2 int>0-p (cc form)
  (assert (eq (cvm2-cc-operand cc) :gt))
  `($bc-gt ,(cvm2-form form) ,(cvm2-quote 0)))

(defcvm2 characterp (cc value) ;; x862-characterp
  (cvm2-boolean-form cc '$bc-characterp (cvm2-form value)))

(defcvm2 endp (cc form)
  (cvm2-boolean-form cc '$bc-endp (cvm2-form form)))

(defcvm2 consp (cc form)
  (cvm2-boolean-form cc '$bc-consp (cvm2-form form)))

(defcvm2 %ilogbitp (cc bitnum value)
  (assert (member (cvm2-cc-operand cc) '(:eq :ne)))
  ;; For some reason, this one is reversed.
  (cvm2-boolean-form cc '$bc-not `($bc-logbitp ,(cvm2-form bitnum) ,(cvm2-form value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; block, tagbody, catch

;; Pass1 converts blocks with no returns into progn, so we don't need to.
(defcvm2 local-block (blocktag body) ;; x862-local-block
  `($bc-block ,(assign-misc-vcell blocktag) ,(cvm2-form body)))

(defcvm2 local-return-from (blocktag value) ;;x862-return-from
  `($bc-return-from ,(get-misc-vcell blocktag) ,(cvm2-form value)))

(defcvm2-fn catch (tag form) $bc-catch)
(defcvm2-fn throw (tag form) $bc-throw)


(defcvm2 local-go (tag) `($bc-go ,(cadr tag)))

(defcvm2 local-tagbody (taglist body) ;;x862-local-tagbody
  (cond ((null taglist)
         ;; this will err out trying to compile TAG-LABEL if there are any labels in it.
         (cvm2-progn (mapcar #'cvm2-form body)))
        (t
         ;; a tag is (tag-sym inner-ref ref-count catch-var T bwd-p).  All of this is internal pass1
         ;; stuff except bwd-p, which is for us, and it's true if this tag was every the target
         ;; of a backward jump, i.e. an actual loop.   This was I think for explicit event checking.
         ;; Anyway, (cadr tag) is modified by x862, so that means we can too..
         (loop for tag in taglist
           do (assert (null (cadr tag)))
           do (setf (cadr tag) (incf *cvm2-tags*)))
         (let* ((remaining-tags (copy-list taglist)) ;; for debugging
                (forms (loop for tag-or-expr in body
                         collect (if (eq (acode-operator-sym tag-or-expr) 'tag-label)
                                   (let ((tag (car (acode-operands tag-or-expr))))
                                     (assert (memq tag remaining-tags))
                                     (setq remaining-tags (delq tag remaining-tags))
                                     (assert (cadr tag))
                                     `($bc-label ,(cadr tag)))
                                   (cvm2-form tag-or-expr)))))
           (assert (null remaining-tags))
           `($bc-tagbody ,@forms)))))


;;  No more.  With alisp target, we can let the host lisp handle the refs.
#+NON-LISP-TARGET
(progn
(defcvm2 local-tagbody (taglist body) ;;x862-local-tagbody
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
                      (and (eq (car form) '$bc-go)
                           (destructuring-bind (tag-var target) (cdr form)
                             (when (eq tag-var catch-tag-var)
                               (decf (car counter))
                               target)))))
               (let ((target (go-target form)))
                 (if target
                   (values target nil)
                   (let* ((target (and (eq (car form) '$bc-progn) (go-target (car (last form))))))
                     (if target
                       (values target (let ((progn (butlast form)))
                                        (if (null (cddr progn)) (cadr progn) progn)))
                       (values nil form))))))))
      (loop for expr in exprs
        as form = (cvm2-form expr)
        as i upfrom 0 do
        (setf (aref codevec i)
              (cond ((eq (car form) '$bc-go)
                     (let ((target (target form)))
                       (if target
                         `($bc-local-go ,target)
                         form)))
                    ((eq (car form) '$bc-if)
                     (destructuring-bind (test yes no) (cdr form)
                       (multiple-value-bind (yes-target yes-form) (target yes)
                         (multiple-value-bind (no-target no-form) (target no)
                           (if (or yes-target no-target)
                             `($bc-local-go-if ,test ,yes-form ,yes-target ,no-form ,no-target)
                             form)))))
                    (t form))))
      ;; number-case generates a GO from deep within a case stmt.
      ;; verify-lambda-list has a GO to outer loop.
      ;(unless (eql 0 (car counter)) (FORMAT T "~&Have ~s missing $LOCAL-GO's" (car counter)))
      (if (eql 0 (car counter))
        `($bc-local-tagbody ,codevec)
        `($bc-tagbody ,catch-tag-var ,codevec)))))


(defcvm2 local-go (tag)
  (destructuring-bind (codevec form-index counter) (cadr tag)
    (incf (car counter))
    `($bc-go ,(get-misc-vcell codevec) ,form-index)))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; conses/uvectors

(defcvm2 (list %temp-list) (forms)
  `($bc-list ,@(mapcar #'cvm2-form forms)))

(defcvm2 list* (arglist)
  ;; TODO: maybe should extract last arg here, so evaluator doesn't have to?
  `($bc-list* ,@(mapcar #'cvm2-form (cvm2-arglist-forms arglist))))

(defcvm2-fn (cons %temp-cons) (x y) $bc-cons)

(defcvm2-fn make-list (size initial-element) $bc-make-list)

;; MIght need a separate $bc-%CAR form if this is ever used to cheat.  Maybe not though.
;; nx1-set-cxr compiles (set-car x y) into (%car (%rplaca x y)), which we should undo.
(defcvm2-fn (car %car) (cons) $bc-car)
(defcvm2-fn (cdr %cdr) (cons) $bc-cdr)
(defcvm2-fn (%rplacd rplacd) (cons value) $bc-rplacd)
(defcvm2-fn set-cdr (cons value) $bc-set-cdr)
(defcvm2-fn (%rplaca rplaca) (cons value) $bc-rplaca)
(defcvm2-fn set-car (cons value) $bc-set-car)


(defcvm2 %gvector (arglist) ;; x862-%gvector
  (let* ((args (cvm2-arglist-forms arglist))
         (subtag-arg (pop args))
         (subtag (acode-fixnum-form-p subtag-arg)))
    `($bc-uvector ,subtag ,@(mapcar #'cvm2-form args))))

(defcvm2 vector (args)
  `($bc-uvector ,(nx-lookup-target-uvector-subtag :simple-vector) ,@(mapcar #'cvm2-form args)))

(defcvm2 %make-uvector (size subtag &optional (init nil init-p)) ;; x862-%alloc-misc
  (if init-p
    `($bc-make-uvector-init ,(cvm2-form size) ,(cvm2-form subtag) ,(cvm2-form init))
    `($bc-make-uvector ,(cvm2-form size) ,(cvm2-form subtag))))

;; JUST use UVREF/UVSET for this?
(defcvm2-fn %svref (vec index) $bc-%svref) ;; need a special one so that can access internal vectors.
(defcvm2-fn %svset (vec index value) $bc-%svset)

(defcvm2-fn (uvref svref) (vec index) $bc-uvref)
(defcvm2-fn (uvset svset) (vec index value) $bc-uvset)
(defcvm2-fn uvsize (vec) $bc-uvsize)

(defcvm2 %typed-uvset (type uvector index newval)
  (let ((subtag (or (acode-fixnum-form-p type)
                     (nx-lookup-target-uvector-subtag
                      (acode-immediate-operand type)))))
    `($bc-subtag-misc-set ,subtag ,(cvm2-form uvector) ,(cvm2-form index) ,(cvm2-form newval))))

(defcvm2 %typed-uvref (type uvector index)
  (let ((subtag (or (acode-fixnum-form-p type)
                    (nx-lookup-target-uvector-subtag
                     (acode-immediate-operand type)))))
    `($bc-subtag-misc-ref ,subtag ,(cvm2-form uvector) ,(cvm2-form index))))
  

(defcvm2-fn aset1 (arr i val) $bc-aset1)
(defcvm2-fn %aref1 (arr i) $bc-aref1)

;; Can probably get by not implementing these for level-0 and then just call AREF!!
(defcvm2-fn general-aref2 (arr i j) $bc-aref2) ;; x862-generic-aref2
(defcvm2-fn general-aref3 (arr i j k) $bc-aref3) ;; really?
(defcvm2-fn general-aset2 (arr i j val) $bc-aset2) ;; x862-general-aset2
(defcvm2-fn general-aset3 (arr i j k val) $bc-aset3) ;; really?

(defcvm2 %scharcode (string index)
  `($bc-char-code ($bc-uvref ,(cvm2-form string) ,(cvm2-form index))))
(defcvm2 %set-scharcode (string index value)
  `($bc-uvset ,(cvm2-form string) ,(cvm2-form index) ($bc-code-char ,(cvm2-form value))))
(defcvm2 %sbchar (string index) `($bc-uvref ,(cvm2-form string) ,(cvm2-form index)))
(defcvm2 %set-sbchar (string index value)
  `($bc-uvset ,(cvm2-form string) ,(cvm2-form index) ($bc-require-character ,(cvm2-form value))))

;; this assumes the value is a lisp object, i.e. doesn't box it.
(defcvm2-fn %fixnum-ref (address offset) $bc-fixnum-ref)
;; This assumes the value is an natural unsigned word, and boxes it.
(defcvm2-fn %fixnum-ref-natural (address offset) $bc-fixnum-ref-natural)
;; Value is an unsigned integer 64, gets unboxed & stored.
(defcvm2-fn %fixnum-set-natural (address offset value) $bc-fixnum-set-natural)


(defcvm2-fn %lisp-word-ref (vec index) $bc-lisp-word-ref)


(defcvm2-fn struct-set (struct offset val) $bc-struct-set)
(defcvm2-fn struct-ref (struct offset) $bc-struct-ref)

(defcvm2-fn %slot-ref (instance idx) $bc-slot-ref)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Types

 ;; x862-lisptag
(defcvm2-fn typecode (node) $bc-typecode)
(defcvm2-fn fulltag (node) $bc-fulltag)
(defcvm2-fn lisptag (node) $bc-lisptag)

(defcvm2-fn gvector-typecode-p (val) $bc-gvector-typecode-p)
(defcvm2-fn ivector-typecode-p (val) $bc-ivector-typecode-p)

(defcvm2 istruct-typep (cc object type-cell-form) ;;x862-istruct-typep
  (let ((type-cell (acode-immediate-operand type-cell-form)))
    (assert (and (consp type-cell) (symbolp (car type-cell))))
    (cvm2-boolean-form cc '$bc-istruct-typep (cvm2-form object) `($bc-quote ,(car type-cell)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; numbers, chars

(defcvm2-fn (add2 %short-float+-2 %double-float+-2 fixnum-add-overflow) (x y) $bc-add2)
(defcvm2-fn (sub2 %short-float--2 %double-float--2 fixnum-sub-overflow) (x y) $bc-sub2)
(defcvm2-fn (mul2 %i* %short-float*-2 %double-float*-2) (x y) $bc-mul2)
(defcvm2-fn (div2 %short-float/-2 %double-float/-2) (x y) $bc-div2)
(defcvm2-fn %iasr (shift x) $bc-iasr)
(defcvm2-fn %ilsr (shift x) $bc-ilsr)
(defcvm2-fn %ilsl (shift x) $bc-ilsl)
(defcvm2-fn (%ilogior2 logior2) (x y) $bc-logior2)
(defcvm2-fn (%ilogxor2 logxor2) (x y) $bc-logxor2)
(defcvm2-fn (logand2 %natural-logand %ilogand2) (x y) $bc-logand2)

 ;; does %ilognot rely on truncating? (most-positive-fixnum)
(defcvm2 (%ilognot lognot) (x) `($bc-sub2 ($bc-quote -1) ,(cvm2-form x)))

(defcvm2-fn logbitp (x y) $bc-logbitp)
(defcvm2-fn %quo2 (x y) $bc-quo2)
(defcvm2-fn (ash fixnum-ash) (x y) $bc-ash) ;; fixnum-ash might rely on truncating

(defcvm2-fn (%single-float %fixnum-to-single) (arg) $bc-single-float)

(defcvm2-fn (%double-float %fixnum-to-double) (arg) $bc-double-float)

(defcvm2-fn %setf-double-float (double val) $bc-setf-double-float)

(defcvm2-fn fixnum-sub-no-overflow (x y) $bc-%i-)
(defcvm2-fn fixnum-add-no-overflow (x y) $bc-%i+)

(defcvm2-fn %word-to-int (word) $bc-word-to-int)

(defcvm2-fn (char-code %char-code) (char) $bc-char-code)

(defcvm2-fn (code-char %code-char %valid-code-char) (code) $bc-code-char)

(defcvm2 base-char-p (cc char)
  (cvm2-boolean-form cc '$bc-base-char-p (cvm2-form char)))

;; See if this ever needs to cheat...
(defcvm2 (%%ineg %ineg minus1) (x) `($bc-sub2 ,(cvm2-quote 0) ,(cvm2-form x)))

(defcvm2-fn (%complex-single-float-realpart %complex-double-float-realpart realpart)
  (arg) $bc-complex-realpart)
(defcvm2-fn (%complex-single-float-imagpart %complex-double-float-imagpart imagpart)
  (arg) $bc-complex-imagpart)

(defcvm2 %make-complex-single-float (real imag)
  `($bc-make-complex ,(cvm2-quote 'single-float) ,(cvm2-form real) ,(cvm2-form imag)))

(defcvm2 %make-complex-double-float (real imag)
  `($bc-make-complex ,(cvm2-quote 'double-float) ,(cvm2-form real) ,(cvm2-form imag)))

(defcvm2 complex (real imag)
  `($bc-make-complex ,(cvm2-quote T) ,(cvm2-form real) ,(cvm2-form imag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; conditionals

(defcvm2-fn if (testform true false) $bc-if)

(defcvm2 or (forms)
  `($bc-or ,@(mapcar #'cvm2-form forms)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MISC

(defcvm2 %err-disp (arglist) ;; x862-%err-disp
  (let ((args (cvm2-arglist-forms arglist)))
    ;;(map nil 'print args)
    (cvm2-progn `(($bc-signalerr ,@(mapcar #'cvm2-form args))
                 ,(cvm2-quote nil)))))

(defcvm2 %badarg2 (badthing goodthing)
  `($bc-signalerr ,(cvm2-quote $XWRONGTYPE)
                  ,(cvm2-form badthing)
                  ,(cvm2-form goodthing)))

(defcvm2-fn %debug-trap (arg) $bc-debug-trap)

(defcvm2-fn %symptr->symvector (symptr) $bc-symptr-to-symvector)
(defcvm2-fn %symvector->symptr (symvector) $bc-symvector-to-symptr)
(defcvm2-fn %symbol->symptr (symbol) $bc-symbol-to-symptr)
  
(defcvm2-fn %current-tcr () $bc-current-tcr)

(defcvm2-fn %unbound-marker () $bc-unbound-marker)
(defcvm2-fn %slot-unbound-marker () $bc-slot-unbound-marker)
(defcvm2-fn %illegal-marker () $bc-illegal-marker)

(defcvm2-fn %current-frame-ptr () $bc-current-frame-ptr)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Foreign fns, macptrs

;; This is just to allow some constant folding...
(defcvm2 %macptrptr% (form)
  (cvm2-form form))

(defcvm2 with-variable-c-frame (size body)
  ;; Undo what pass1 did...
  (assert (eq (acode-operator-sym body) 'let*))
  (destructuring-bind (vars vals let-body . ignore) (acode-operands body)
    (declare (ignore ignore))
    (assert (= (length vars) (length vals) 1))
    (let ((var (car vars)) (val (car vals)))
      (assert (eq (acode-operator-sym val) '%foreign-stack-pointer))
      (setq body let-body)
      `($bc-with-variable-c-frame ,(cvm2-form size) ,(cvm2-binding-var var) ,(cvm2-form let-body)))))

(defcvm2 %foreign-stack-pointer ()
  (error "%foreign-stack-pointer not within with-variable-c-frame!"))


;; To avoid consing macptrs, pass1 deconstructs stuff like %int-to-ptr into a series of immediate
;; operations that then end up calling %CONSMACPTR% at the end.  For now re-construct that.
(defcvm2 %consmacptr% (arg) ;;x862-%consmacptr%
  (ecase (acode-operator-sym arg)
    (%immediate-int-to-ptr 
     `($bc-int-to-macptr ,@(mapcar #'cvm2-form (acode-operands arg)))) ;; %int-to-ptr
     ;; Argh, should open code it, don't really need this
    (%immediate-inc-ptr
     `($bc-inc-macptr ,@(mapcar #'cvm2-form (acode-operands arg))))
    (immediate-get-ptr
     `($bc-macptr-get ,@(mapcar #'cvm2-form (acode-operands arg)) :pointer))))

(defcvm2-fn %immediate-ptr-to-int (form) $bc-macptr-to-int)

(defcvm2 immediate-get-xxx (bits macptr offset)
  (let* ((fixnump (logbitp 6 bits))
         (signed (logbitp 5 bits))
         (size (logand 15 bits))
         (ffsize (if fixnump
                   (target-word-size-case
                    (64 :int64)
                    (32 :int32))
                   (ecase size
                     (8 (if signed :int64 :uint64))
                     (4 (if signed :int32 :uint32))
                     (2 (if signed :int16 :uint16))
                     (1 (if signed :int8 :uint8))))))
    `($bc-macptr-get ,(cvm2-form macptr) ,(cvm2-form offset) ,ffsize)))

(defcvm2 %get-double-float (macptr offset)
  `($bc-macptr-get ,(cvm2-form macptr) ,(cvm2-form offset) :double))

(defcvm2 %get-single-float (macptr offset)
  `($bc-macptr-get ,(cvm2-form macptr) ,(cvm2-form offset) :float))

(defcvm2-fn %get-bit (macptr bit-offset) $bc-macptr-get-bit)
(defcvm2-fn %set-bit (macptr bit-offset val) $bc-macptr-set-bit)

(defcvm2-fn %new-ptr (size clear-p) $bc-new-macptr) ;x862-%new-ptr
  
(defcvm2 %immediate-int-to-ptr (arg)
  (error "%immediate-in-to-ptr Not supported: ~s" arg))

(defcvm2-fn %fixnum-ref-double-float (base index) $bc-fixnum-ref-double-float)
(defcvm2-fn %fixnum-set-double-float (base index val) $bc-fixnum-set-double-float)


(defcvm2 %ptr-eql (cc form1 form2)
  (cvm2-boolean-form cc '$bc-macptr-eql (cvm2-form form1) (cvm2-form form2)))


(defcvm2-fn %setf-macptr (macptr val) $bc-setf-macptr)


(defcvm2 %immediate-set-xxx (bits macptr offset val) ;x862-%immediate-set-xxx
  ;;(x862-%immediate-store seg vreg xfer bits ptr offset val)
  (let* ((size (logand #xF bits)) ;; 0 means ...
         (signed (not (logbitp 5 bits)))
         (ffsize (ecase size
                   (8 (if signed :int64 :uint64))
                   (4 (if signed :int32 :uint32))
                   (2 (if signed :int16 :uint16))
                   (1 (if signed :int8 :uint8))
                   (0 (assert signed) :pointer))))
    `($bc-macptr-set ,(cvm2-form macptr) ,(cvm2-form offset) ,ffsize ,(cvm2-form val))))

(defcvm2 %set-double-float (macptr offset val)
  `($bc-macptr-set ,(cvm2-form macptr) ,(cvm2-form offset) :double ,(cvm2-form val)))

(defcvm2 %set-single-float (macptr offset val)
  `($bc-macptr-set ,(cvm2-form macptr) ,(cvm2-form offset) :float ,(cvm2-form val)))

(defcvm2 builtin-call (index arglist);; x862-builtin-call
  ;; I think this was just an optimization to save space by having a subprim call the function
  (let* ((args (cvm2-arglist-forms arglist))
         (index-val (acode-fixnum-form-p index))
         (builtin (svref %builtin-functions% index-val))
         (op (cdr (assoc builtin '((>-2 . $bc-gt)
                                   (<-2 . $bc-lt)
                                   (=-2 . $bc-=)
                                   (sequence-type . $bc-seqtype)
                                   (eql . $bc-eql)
                                   (ash . $bc-ash)
                                   (%aset1 . $bc-aset1)
                                   (%aref1 . $bc-aref1)
                                   (length . $bc-length))))))
    (assert op () "Unknown builtin ~s" builtin)
    `(,op ,@(mapcar #'cvm2-form args))))


(defcvm2 %reference-external-entry-point (arg)
  `($bc-%reference-external-entry-point ,(cvm2-form arg)))


(defcvm2 ff-call (address argspecs argvals resultspec &optional monitor)
  (declare (ignore monitor))
  (assert (not (find :void argspecs)))
  (flet ((ffspec (spec)
           (case spec
             ((nil) (target-word-size-case
                     (64 :int64)
                     (32 :int32)))
             (:signed-byte :int8)
             (:unsigned-byte :uint8)
             (:signed-halfword :int16)
             (:unsigned-halfword :uint16)
             (:signed-fullword :int32)
             (:unsigned-fullword :uint32)
             (:signed-doubleword :int64)
             (:unsigned-doubleword :uint64)
             (:single-float :float)
             (:double-float :double)
             (:address :pointer)
             (:void :void)
             (t (require-type spec 'unsigned-byte)))))
    (let* ((argspecs (map 'list #'ffspec argspecs))
           (resultspec (ffspec resultspec))
           (address-form (cvm2-form address))
           (arg-forms (list argspecs (map 'list #'cvm2-form argvals) resultspec)))
      (assert (not (typep resultspec 'unsigned-byte)))
      #+NO (format t "~&FF argspecs: ~s => ~s~%" (remove-duplicates argspecs) resultspec)
      (if (and (eq (first address-form) '$bc-funcall)
               (equal (second address-form) '($bc-quote cvm-%kernel-import))
               (eq (car (third address-form)) '$bc-quote))
        `($bc-kernel-call ,(symbol-name (cadr (third address-form))) ,@arg-forms)
        (progn
          ;;  This only happens once: ($BC-%REFERENCE-EXTERNAL-ENTRY-POINT ($BC-QUOTE (#:LOAD-TIME-EVAL (FUNCALL #<Anonymous Function #x302002A7362F>))))
          ;;  in sockets.lisp.
          ;;(unless (equal (car address-form) '$bc-symbol-value)
          ;;  (format *trace-output* "~&;;; *** Different FF-CALL ~s" address-form))
          `($bc-ff-call ,address-form ,@arg-forms))))))

