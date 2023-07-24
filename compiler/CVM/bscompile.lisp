(in-package :ccl)

;  (trace  :before (lambda (fn afunc &rest flags) (assert (eq fn 'x862-compile)) flags (setq *last-afunc afunc)) x862-compile)
;; TODO:  pass the "vreg" arg in, it says whether it's being evaluated for a vlaue, and it's really useful
;;(fcomp-file src (or compile-file-original-truename (namestring orig-src)) compile-file-original-buffer-offset lexenv)

;;; *** TODO:  A "bslambda" is really compiled code.  It's not a lambda, it's a readable
;;; representation of a function.  We don't do (FUNC (BSLAMBDA)).  LOADING A BSLAMBDA SHOULD
;;; just make a function!!!  Get rid of BSLAMBDA entirely and just make a $BS-COMPILED-FUNCTION
;;; operator... outputting an lfun should output ($BS-COMPILED-FUNCTION name argspecs etc)
;;;   ccl-function object should have a ccl-function-source which can be a list with all the info

(defvar *ev2-cur-afunc*)

(defvar *ev2-lex-vars*)
(defvar *ev2-tags*)

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


(defparameter *ev2-prototype* (nfunction ev2-func
                                (lambda (&rest args)
                                  (declare (ignore args))
                                  (error "Can't eval: ~s" 'ev2-lambda))))

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

(defmacro bslambda-bits (bslambda) `(car (last (third ,bslambda))))

;;; *** TODO: rename to cvm2-
(defun ev2-compile (afunc &optional lambdaform record-symbols)  ;;x862-compile
  (dolist (a (afunc-inner-functions afunc))
    (unless (afunc-lfun a)
      (assert (eq (afunc-parent a) afunc))
      (ev2-compile a (if lambdaform (afunc-lambdaform a)) record-symbols)))

  #+NO (pprint (decomp-acode (afunc-acode afunc)))

  (let* ((inherited-vars (afunc-inherited-vars afunc))
         (fbits (afunc-bits afunc))
         (bslambda 
          (let ((*ev2-cur-afunc* afunc)
                (acode (afunc-acode afunc)))
            (assert (eq (acode-operator-sym acode) 'lambda-list))
            (when inherited-vars
              (assert (afunc-parent afunc))
              (assert (let* ((outer-afunc (afunc-parent afunc))
                             (outer-vars (afunc-all-vars outer-afunc))
                             (outer-inh (afunc-inherited-vars outer-afunc)))
                        (loop for v in inherited-vars
                          always (let ((outer-var (ev2-var-inherited-from v)))
                                   (and outer-var
                                        (or (member outer-var outer-vars)
                                            (member outer-var outer-inh))))))))
            (apply #'ev2-lambda-form (afunc-name afunc) fbits inherited-vars (acode-operands acode)))))

    (when (and (logbitp $fbitnextmethargsp fbits)
               (not (logbitp $fbitmethodp fbits)))
      (break "When does this happen?")
      (let ((parent (afunc-parent afunc)))
        (when parent
          (bitsetf $fbitnextmethargsp (afunc-bits parent)))))

    ;; could make the xfunction, would get a bit more error checking.
    (setf (afunc-lfun afunc)
          (ev2-make-lfun (afunc-name afunc) bslambda))

    ;; now that we have an lfun, fixup any forward refs to the fn.
    (loop for ref in (afunc-fwd-refs afunc)
      do (assert (equal ref `($bs-quote ,afunc)))
      do (setf (cadr ref) (afunc-lfun afunc))))
  afunc)

;; x862-lambda (our ev2-lambda-form) returns this


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
               nconc (list `(setf (gethash ',operator-name *ev2-specials*) fn)))
             (list `(setf (gethash ',operator-name-or-names *ev2-specials*) fn)))))))

(defmacro defev2-fn (operator arglist runtime-op)
  (assert (every (lambda (x) (and (symbolp x) (not (eql #\& (char (string x) 0))))) arglist))
  (check-type runtime-op symbol)
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


(defun ev2-lambda-form (name fbits inh req opt rest keys auxen acode p2decls &aux lexpr (bits 0))
  (declare (ignorable p2decls)) ;; stuff like tail-call-allow, safety, trust-declarations.
  (assert (every #'ev2-var-inherited-from inh))
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

  (let* ((*ev2-lex-vars* nil)
         (*ev2-tags* 0)
         (argspecs (list
                    ;; inh
                    (let ((num-inh (length inh)))
                      (setf (ldb $lfbits-numinh bits) (min num-inh (ldb $lfbits-numinh -1)))
                      (mapcar #'ev2-binding-var inh))
                    ;; req
                    (let ((num-req (length req)))
                      (when (and (logbitp $fbitmethodp fbits) ;; lie, to match x86...
                                 (logbitp $fbitnextmethp fbits)) ;; lie about hte extra arg.
                        (assert (> num-req 0))
                        (decf num-req))
                      (setf (ldb $lfbits-numreq bits) (min num-req (ldb $lfbits-numreq -1)))
                      (mapcar #'ev2-binding-var req))
                    ;; opt
                    (when opt
                      (destructuring-bind (opt-vars opt-inits opt-supp-vars) opt
                        (let ((num-opt (length opt-vars)))
                          (assert (= num-opt (length opt-inits) (length opt-supp-vars)))
                          (setf (ldb $lfbits-numopt bits) (min num-opt (ldb $lfbits-numopt -1)))
                          (mapcar (lambda (var init supp)
                                    (unless (and (nx-null init) (not supp))
                                      (bitsetf $lfbits-optinit-bit bits))
                                    (list (ev2-binding-var var)
                                          (ev2-form init)
                                          (and supp (ev2-binding-var supp))))
                                  opt-vars opt-inits opt-supp-vars))))
                    ;; rest
                    (when rest
                      (bitsetf (if lexpr $lfbits-restv-bit $lfbits-rest-bit) bits)
                      (ev2-binding-var rest))
                    ;; keys
                    (when keys
                      (bitsetf $lfbits-keys-bit bits)
                      (destructuring-bind (allow-other-keys-p keyvars keysupp keyinits keykeys) keys
                        (assert (= (length keyvars) (length keysupp) (length keyinits) (length keykeys)))
                        (when allow-other-keys-p (bitsetf $lfbits-aok-bit bits))
                        (cons allow-other-keys-p
                              (map 'list (lambda (key var init supp)
                                           (list (ev2-quote key) ;; Need to quote it so gets converted
                                                 (ev2-binding-var var)
                                                 (ev2-form init)
                                                 (and supp (ev2-binding-var supp))))
                                   keykeys keyvars keyinits keysupp))))
                    ;; flags
                    bits))
         (body (ev2-form acode)))
    (when lexpr
      (let ((rest-var (get-lex-vcell rest)))
        (setq body (ev2-progn
                    `(($BS-LSET ,rest-var ($BS-LEXPR-ARGS ,rest-var))
                      ,body)))))
    (assert (every #'(lambda (v) (or (member (car v) inh)
                                     (member (car v) (afunc-all-vars *ev2-cur-afunc*))
                                     (consp (car v)))) ;; block tag
                   *ev2-lex-vars*))
    `(bslambda ,(ev2-quote name)
               ,argspecs
               ,body
               ,(length *ev2-lex-vars*))))


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
  `(,(if spread-p '$BS-apply '$BS-funcall)
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
  (member op '($BS-not $BS-yes $BS-eq $BS-gt $BS-le $BS-lt $BS-ge $BS-characterp $BS-endp
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

;; lots of constant folding opportunities here see x862-eq-test
(defev2 (eq neq) (cc form1 form2)
  (let ((form `($bs-eq ,(ev2-form form1) ,(ev2-form form2))))
    (ecase (ev2-cc-operand cc)
      (:EQ form)
      (:NE `($bs-not ,form)))))

(defev2 (numcmp short-float-compare double-float-compare %i<> %natural<>) (cc form1 form2) ;;x862-numcmp
  (let ((forms (list (ev2-form form1) (ev2-form form2))))
    (ecase (ev2-cc-operand cc)
      (:EQ `($bs-= ,@forms))
      (:NE `($bs-not ($bs-= ,@forms)))
      (:GT `($bs-gt ,@forms))
      (:LE `($bs-not ($bs-gt ,@forms)))
      (:LT `($bs-lt ,@forms))
      (:GE `($bs-not ($bs-lt ,@forms))))))

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


(defev2 local-go (tag) `($BS-GO ,(cadr tag)))

(defev2 local-tagbody (taglist body) ;;x862-local-tagbody
  (cond ((null taglist)
         ;; this will err out trying to compile TAG-LABEL if there are any labels in it.
         (ev2-progn (mapcar #'ev2-form body)))
        (t
         ;; a tag is (tag-sym inner-ref ref-count catch-var T bwd-p).  All of this is internal pass1
         ;; stuff except bwd-p, which is for us, and it's true if this tag was every the target
         ;; of a backward jump, i.e. an actual loop.   This was I think for explicit event checking.
         ;; Anyway, (cadr tag) is modified by x862, so that means we can too..
         (loop for tag in taglist
           do (assert (null (cadr tag)))
           do (setf (cadr tag) (incf *ev2-tags*)))
         (let* ((remaining-tags (copy-list taglist)) ;; for debugging
                (forms (loop for tag-or-expr in body
                         collect (if (eq (acode-operator-sym tag-or-expr) 'tag-label)
                                   (let ((tag (car (acode-operands tag-or-expr))))
                                     (assert (memq tag remaining-tags))
                                     (setq remaining-tags (delq tag remaining-tags))
                                     (assert (cadr tag))
                                     `($BS-LABEL ,(cadr tag)))
                                   (ev2-form tag-or-expr)))))
           (assert (null remaining-tags))
           `($bs-tagbody ,@forms)))))


;;  No more.  With alisp target, we can let the host lisp handle the refs.
#+NON-LISP-TARGET
(progn
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
)
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
    `($BS-UVECTOR ,subtag ,@(mapcar #'ev2-form args))))

(defev2 vector (args)
  `($BS-UVECTOR ,(nx-lookup-target-uvector-subtag :simple-vector) ,@(mapcar #'ev2-form args)))

(defev2 %make-uvector (size subtag &optional (init nil init-p)) ;; x862-%alloc-misc
  (if init-p
    `($BS-make-uvector-init ,(ev2-form size) ,(ev2-form subtag) ,(ev2-form init))
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

(defev2 %scharcode (string index)
  `($bs-char-code ($bs-uvref ,(ev2-form string) ,(ev2-form index))))
(defev2 %set-scharcode (string index value)
  `($bs-uvset ,(ev2-form string) ,(ev2-form index) ($bs-code-char ,(ev2-form value))))
(defev2 %sbchar (string index) `($bs-uvref ,(ev2-form string) ,(ev2-form index)))
(defev2 %set-sbchar (string index value)
  `($bs-uvset ,(ev2-form string) ,(ev2-form index) ($bs-require-character ,(ev2-form value))))

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
(defev2-fn (logand2 %natural-logand %ilogand2) (x y) $BS-LOGAND2)

 ;; does %ilognot rely on truncating? (most-positive-fixnum)
(defev2 (%ilognot lognot) (x) `($bs-sub2 ($bs-quote -1) ,(ev2-form x)))

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

(defev2-fn %get-bit (macptr bit-offset) $BS-macptr-get-bit)
(defev2-fn %set-bit (macptr bit-offset val) $BS-macptr-set-bit)

(defev2-fn %new-ptr (size clear-p) $BS-NEW-MACPTR) ;x862-%new-ptr
  
(defev2 %immediate-int-to-ptr (arg)
  (error "%immediate-in-to-ptr Not supported: ~s" arg))

(defev2-fn %fixnum-ref-double-float (base index) $bs-fixnum-ref-double-float)
(defev2-fn %fixnum-set-double-float (base index val) $bs-fixnum-set-double-float)


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

(defev2 builtin-call (index arglist);; x862-builtin-call
  ;; I think this was just an optimization to save space by having a subprim call the function
  (let* ((args (ev2-arglist-forms arglist))
         (index-val (acode-fixnum-form-p index))
         (builtin (svref %builtin-functions% index-val))
         (op (cdr (assoc builtin '((>-2 . $bs-gt)
                                   (<-2 . $bs-lt)
                                   (=-2 . $BS-=)
                                   (sequence-type . $bs-seqtype)
                                   (eql . $bs-eql)
                                   (ash . $bs-ash)
                                   (%aset1 . $bs-aset1)
                                   (%aref1 . $bs-aref1)
                                   (length . $bs-length))))))
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
            (format *trace-output* "~&;;; *** Different FF-CALL ~s" address-form))
          `($BS-FF-CALL ,address-form ,@arg-forms))))))

