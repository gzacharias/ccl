(in-package :ccl)

(defparameter *modules-not-for-cvm*
  '(;l1-lisp-threads l1-processes l1-sockets    ;sockets
    ;; l1-cl-package
    edit-callers
    cover
    leaks
    core-files
    dominance
    backtrace-lds
    ;; compiler non-vm backends
    VREG
    VINSN
    REG))

;; For testing, integrate back into general setup later
(defparameter *modules-to-compile*
  '(LEVEL-1
    L1-CL-PACKAGE
    L1-UTILS
    L1-INIT
    L1-SYMHASH
    L1-NUMBERS
    L1-APRIMS
    ;X86-CALLBACK-SUPPORT
    L1-CALLBACKS
    L1-SORT
    
    lists  ;; lib

    sequences  ;; lib


    L1-DCODE

    L1-CLOS-BOOT
    hash ;; lib
    
    L1-CLOS

    defstruct;; lib
    dll-node ;;lib
    
    L1-UNICODE
    L1-STREAMS


    LINUX-FILES
    chars ;;lib
    
    L1-FILES
    ;;(provide "SEQUENCES")
    ;;(provide "DEFSTRUCT")
    ;;(provide "CHARS")
    ;;(provide "LISTS")
    ;;(provide "DLL-NODE")
    
    L1-TYPESYS
    SYSUTILS


    ;;X86-THREADS-UTILS
    L1-LISP-THREADS
    L1-APPLICATION
    L1-PROCESSES
    L1-IO
    L1-READER
    L1-READLOOP
    L1-READLOOP-LDS
    L1-ERROR-SYSTEM
    L1-EVENTS
    ;X86-TRAP-SUPPORT
    
    
    L1-FORMAT
    L1-SYSIO
    L1-PATHNAMES
    L1-BOOT-LDS
    L1-BOOT-1
    
    
    L1-BOOT-2  ;; defs stuff but also loads:
    ;X86-ERROR-SIGNAL
    L1-ERROR-SIGNAL
    L1-SOCKETS
    
    ;; loads and provides these
    SORT
    NUMBERS

    SUBPRIMS
    ;X8664-ARCH
    CVM-ARCH
    VREG
    VINSN
    REG

    BACKEND
    NX2

    ; (PROVIDE X862)
    ACODE-REWRITE
    NX

    ;X862
    CVM2

    level-2
    macros
    setf
    setf-runtime
    format
    streams
    optimizers
    defstruct-macros
    defstruct-lds
    nfcomp ;; we need the front end.
    BSFCOMP
    backquote
    backtrace-lds
    backtrace
    read
    arrays-fry
    apropos
    source-files

    ;X86-DISASSEMBLE
    ;X86-LAPMACROS
    ;x86-watch
    foreign-types
    ;;;; (install-standard-foreign-types *host-ftd*)
    ;FFI-DARWINX8664
    
    ;; Knock wood: all standard reader macros and no non-standard
    ;; reader macros are defined at this point.
    ;;;; (setq *readtable* (copy-readtable *readtable*))
    
    db-io
    ;;;; (canonicalize-foreign-type-ordinals *host-ftd*)
    
    case-error
    ENCAPSULATE
    METHOD-COMBINATION
    misc
    pprint
    dumplisp
    pathnames
    time
    compile-ccl
    systems
    arglist
    edit-callers
    describe
    swink
    cover
    leaks
    core-files
    dominance
    swank-loader
    remote-lisp
    mcl-compat
    loop
    ccl-export-syms
    version
    jp-encode
    cn-encode
    lispequ
    sockets
    L1-BOOT-3

    CVM-BACKEND
    nxenv
    HASHENV
    NUMBER-MACROS
    NUMBER-CASE-MACRO
    arch
    PRINT-DB
    PREPARE-MCL-ENVIRONMENT
    ))



(defun test-vm (&optional force)
  ;; Don't really understand the intended way of doing this.  Any attempt to
  ;; use a new target ends up calling FIND-BACKEND, but there is no cvm backend until
  ;; these files are loaded, so just do it.
  (load "ccl:compiler;cvm;cvm-arch.lisp") ;; on cvm, this gets loaded by l1-boot-2.lisp
  ;; this normally gets loaded by loading CVM2.lisp, after loading above.  Have to compile it so require can find it.
  (compile-file "ccl:compiler;cvm;cvm-backend.lisp" :output-file "ccl:bin;cvm-backend" :verbose t :load t)

  (let ((*warn-if-redefine-kernel* nil))
    (if (eq force :full)
      ;; This is overkill, but go through all the required/provided modules to make sure
      ;; compile-time env is all there and up to date.
      (compile-ccl t)
      ;; Else just load stuff we redefined.  Until build a new lisp.
      (let ((*warn-if-redefine-kernel* nil))
        (load "ccl:lib;systems.lisp") ;; make sure we have the latest, avoid bootstrapping issuess.
        (load "ccl:lib;macros.lisp")
        (load "ccl:lib;foreign-types.lisp")
        (load "ccl:lib;db-io.lisp")
        (load "ccl:library;sockets.lisp")
        ;(load "ccl:lib;nfcomp.lisp")
        ;(load "ccl:lib;compile-ccl.lisp")
        )))

  ;; Compile level-0
  (let* ((*build-time-optional-features* nil)
         (*features* *features*)
         (*save-source-locations* NIL #+no *ccl-save-source-locations*)
         (*cerror-on-constant-redefinition* t)
         ;; Once we get into lists and other lib files, macros get redefined at compile-time
         ;(*warn-if-redefine-kernel* t)
         (*warn-if-redefine-kernel* nil)
         (*package* (find-package :ccl))
         (*save-doc-strings* t)
         (*fasl-save-doc-strings* t))
    (with-global-optimization-settings ()
      (flet ((fcomp (srcs outpath)
               (unless (consp srcs) (setq srcs (list srcs)))
               (let* ((src (car srcs))
                      (output (merge-pathnames outpath src)))
                 (when force (assert (not (probe-file output)))) ;; Check for duplicate filenames...
                 (when (or force
                           (not (probe-file output))
                           (let ((outdate (file-write-date output)))
                             (some (lambda (src) (> (file-write-date src) outdate)) srcs)))
                   (setq *nx-speed* (max 1 *nx-speed*))
                   (setq *nx-safety* (min 1 *nx-safety*))
                   ;; This sets up the target:: and os:: package nicknames and *target-ftd*
                   (with-cross-compilation-target (:darwincvm)
                     ;; compile-file doesn't like to replace non-fasl file
                     (when (probe-file output) (delete-file output))
                     (compile-file src :target :darwincvm :features nil :output-file output :verbose t))))))
        ;; TODO: Maybe should make a file, LEVEL-0.LISP that just sets *level-0-files*, which can then be loaded,
        ;; so don't rely on contents of directories..
        (ensure-directories-exist "ccl:cvmsrcs;level-0;")
        (let ((outpath (merge-pathnames "ccl:cvmsrcs;level-0;" (backend-target-fasl-pathname *cvm-backend*))))
          (when force (mapcar #'delete-file (directory (make-pathname :name :wild :defaults outpath))))
          (dolist (dir '("ccl:level-0;" "ccl:level-0;CVM;"))
            (loop for src in (sort (directory (merge-pathnames dir "*.lisp")) #'string< :key #'namestring)
              do (fcomp src outpath))))
        (ensure-directories-exist "ccl:cvmsrcs;")
        (let ((outpath (merge-pathnames "ccl:cvmsrcs;" (backend-target-fasl-pathname *cvm-backend*))))
          (when force (mapcar #'delete-file (directory (make-pathname :name :wild :defaults outpath))))
          (loop for module in *modules-to-compile*
            if (member module *modules-not-for-cvm*)
            do (format t "~&IGNORING ~s" module)
            ;; Ignore the requested fasl dir, we're putting everything in one dir
            else do (fcomp (caddr (assoc module *ccl-system*)) outpath)))))))

        
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
  
;; So all this needs to get vm versions, or be pre-built into the vm.
;"level-0/X86/X8664/x8664-bignum" "level-0/X86/x86-array" "level-0/X86/x86-clos" "level-0/X86/x86-def" "level-0/X86/x86-float"
;"level-0/X86/x86-hash" "level-0/X86/x86-io""level-0/X86/x86-misc""level-0/X86/x86-numbers""level-0/X86/x86-pred"
;"level-0/X86/x86-symbol""level-0/X86/x86-utils"

;"level-0/l0-aprims""level-0/l0-array""level-0/l0-bignum32" "level-0/l0-bignum64" "level-0/l0-cfm-support"
;"level-0/l0-complex""level-0/l0-def""level-0/l0-error""level-0/l0-float""level-0/l0-hash""level-0/l0-init""level-0/l0-int"
;"level-0/l0-io""level-0/l0-misc""level-0/l0-numbers""level-0/l0-pred""level-0/l0-symbol""level-0/l0-utils""level-0/nfasload"


#+not-used
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
              as output-file = (merge-pathnames (backend-target-fasl-pathname *cvm-backend*) file)
              ;; Compile file complains if it's not a fasl file.
              when (probe-file output-file) do (delete-file output-file)
              do (compile-file file
                               :target :cvm
                               :output-file output-file
                               :verbose verbose)))
        (setf (current-directory) cd)))))

  

;; Use the first pass of the file compiler, but do our own alternate output.
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
                 (every #'null (butlast argspecs))
                 (zerop nlocals)
                 (eql (length body) 3)
                 (eq (car body) '$bs-funcall)
                 (equal (cadr body) '($bs-quote ccl::set-package))
                 (eq (car (caddr body)) '$bs-quote))
        (cadr (caddr body))))))
        

(defun ev2-output-compiled-file (toplevel-forms hash output-file)
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
  (let ((bslambda (ev2-lfun-bslambda fn)))
    ;;; ***TODO: Currently we're not generating/tracking the lfun-bits!!!
    ;;; Stick them in the bslambda, since can't give the lfun incorrect lfun-bits!
    ;;; Or have an XFUNCTION type that we use.
    (let ((*ev2-bsquote* nil))
      `($fs-init-function ,(ev2-maybe-store '($fs-cons-function) store-index)
                          ($fs-init-bslambda ,(ev2-maker-form bslambda))))))

