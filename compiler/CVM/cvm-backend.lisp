;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")

;; nothing really loads this automatically.
;; or, see target-xcompile-ccl which first updates *sysdef-modules* (systems
;;  and 
;;; FILE COMPILER sets up features based on target
;; host with *target-backend* <- *host-backend*, so
;; 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "NXENV")
  ;; This would be lib;cmvenv.lisp, but there is nothing we want to put there.
  ;(require "CVMENV")
  )

;; This should be defined in backend.lisp but doesn't seem to get loaded in time,
;;  so screw it..
(defconstant platform-cpu-cvm (ash 4 3))

(next-nx-defops)

;; To be defined in the host.
(declaim (ftype function
                cvm-%kernel-import
                cvm-ffi-function
                cvm-external-call
                cvm-access-foreign-record
                (setf cvm-access-foreign-record)
                cvm-access-foreign-array
                ;(setf cvm-access-foreign-array)
                cvm-foreign-size
                %get-native-value
                %store-native-value-ptr))

(defstruct (cvm-foreign-pointer-type (:include foreign-pointer-type)
                                     (:constructor make-cvm-foreign-pointer-type
                                                   (&key name bits)))
  (name nil))

;; this takes care of %foreign-type-or-record
(defun %defer-load-record (name)
  (assert (keywordp name))
  (if (eq name :address)
    (make-cvm-foreign-pointer-type :name name)
    (unless (info-foreign-type-definition name)
      (make-foreign-record-type :kind :struct
                                :name name))))

(defun %deferred-foreign-access-form (base-form accessor bit-offset)
  `(cvm-access-foreign-record ,base-form ',accessor ,bit-offset))

(defun %deferred-foreign-array-access-form (base-form name index-form)
  (assert (keywordp name))
  `(cvm-access-foreign-array ,base-form ',name ,index-form))

(defun %deferred-foreign-size-form (type units)
  `(values (ceiling
            (cvm-foreign-size ',type)
            ,(ecase units (:bits 1) (:bytes 8) (:words 32)))))


(defun %deferred-foreign-init-forms (ptr record-name inits)
  (assert (keywordp record-name))
  (if (null (cdr inits))
    `((setf ,(%deferred-foreign-access-form ptr record-name 0) ,(car inits)))
    (loop with prefix = (string record-name)
      for (key init) on inits by #'cddr
      collect `(setf ,(%deferred-foreign-access-form ptr
                                                     (make-keyword (%str-cat prefix "." (string key)))
                                                     0)
                     ,init))))

;;; Below was first attempt, might want to back out of some of this:

;; totally faking it, just  so can read everything in.  Will have to look
;; and see what mess gets generated.
(defun cvm-ffi-type (string)
  (declare (ignore string))
  (make-foreign-pointer-type))
;(%foreign-type-or-record type-name)
;; SYM will get a macro definition of %external-call-expander
;; so that calls to it `(external-call ,entry-name ,stuff-ufrom-args-and-result)

(defun cvm-ffi-function (sym)
  (declare (ignore sym))
  (make-external-function-definition))

(defun cvm-ffi-expander (name &rest args)
  `(cvm-external-call ',name ,@args))

;; x8664::expand-ff-call
(defun cvm-expand-ff-call (callform args)
  ;;(error "who calls this")
  ;;; *** If this is not enough, do the comiler macro and skip this.
  (let ((ffn (cadr callform)))
    (when (and (consp ffn) (eq (car ffn) '%kernel-import))
      (destructuring-bind (offset) (cdr ffn)
        (assert (symbolp offset))
        (setq callform (list* (car callform)
                              `(cvm-%kernel-import ',offset)
                              (cddr callform))))))
  ;; Cheat a little.  All this does is standardize the keywords, like :int => :signed-fullword
  (let ((*target-ftd* (backend-target-foreign-type-data *host-backend*)))
    (funcall (ftd-ff-call-expand-function *target-ftd*)
             callform args)))

;; DOn't want to redefine ccl function, so..
(define-compiler-macro %kernel-import (offset)
  (when (eq *target-backend* *cvm-backend*)
    (break "who still calls this? ~s" offset)))


;; l1-aprims does this:
;(defpackage #.(ftd-interface-package-name (backend-target-foreign-type-data *target-backend*))
;  (:nicknames "OS")
;  (:use "COMMON-LISP"))
; BUT compile-file does not bind *target-backend* at read time, only compile-named-function binds it.
;; which means it defines the package that it was compiled for when it's loaded.
;;  but if we're compiling for a different target, and we need the package to
;;  be set up for #$ ...
(eval-when (eval load) ;; doing this at compile time interferes with cross compilation package manipulation
  (or (find-package "CVMDARWIN-FFI")
      (make-package "CVMDARWIN-FFI" :use "COMMON-LISP")))

(defparameter *cvm-ftd*
  (make-ftd :interface-db-directory 'unknown  ;;"ccl:darwin-cvm-headers;"
            ;; (if (eq backend *host-backend*)
            ;;   "ccl:darwin-x86-headers64;"
            ;;   "ccl:cross-darwin-x86-headers64;")
            :interface-package-name "CVMDARWIN-FFI"
            :attributes (list :bits-per-word 64
                              :signed-char t
                              :struct-by-value t
                              :prepend-underscore t
                              :defer-to-runtime t
                              :type-lookup 'cvm-ffi-type
                              :function-lookup 'cvm-ffi-function
                              :call-expander 'cvm-ffi-expander
                              )
            :ff-call-expand-function 'cvm-expand-ff-call
            :ff-call-struct-return-by-implicit-arg-function 'unknown ;; (Intern ...)
            :callback-bindings-function 'unknown ;; (intern ..
            :callback-return-value-function 'unknown;; (intern
            ))
;;(setf (gethash :array (ftd-translators *cvm-ftd*))
(setf (info-foreign-type-translator :array *cvm-ftd*)
      (lambda (whole env)
        (declare (ignore whole env))
        (make-foreign-array-type :element-type (make-foreign-pointer-type))))
(setf (info-foreign-type-translator :* *cvm-ftd*)
      (lambda (whole env)
        (declare (ignore whole env))
        (make-foreign-pointer-type)))

;; L1-init calls (%foreign-type-or-record-size :timeval :bytes) At READ time. Kludge it,
;;  makes sure to give it enough bits for any reasonable :timeval structure...
(setf (info-foreign-type-struct :timeval *cvm-ftd*)
      (make-foreign-record-type :kind :struct
                                :name :timeval
                                :bits 1024))

;;; **** CHEAT.  Could copy over all integer types.
;;; Assume all integer types will remain integer types on that target...
;;; -- Once everything compiles ok,  just see which ones get looked up
;; definitely need :mach_msg_type_number_t
(maphash (lambda (k v) (when (typep v 'foreign-integer-type)
                         ;; This might be messing up the ordinal scheme,
                         ;; figure out what that's about!
                         (setf (info-foreign-type-definition k *cvm-ftd*) v)
                         (setf (info-foreign-type-kind k *cvm-ftd*) 
                               (info-foreign-type-kind k *host-ftd*))))
         (ftd-definitions *host-ftd*))


;;I don't really understand how these features are supposed to get trigger,
;; when the target doesn't exist yet and everybody wants to FIND-BACKEND!
;; #+darwincvm-target
(defparameter *darwincvm-backend*
  (make-backend :lookup-opcode 'unknown ;;#'arm::lookup-arm-instruction
		:lookup-macro 'unknown ;; #'false
		:lap-opcodes #() ;; not referrenced - can't use 'unknown because of type decl
                :define-vinsn 'unknown ;;'%define-arm-vinsn
                :platform-syscall-mask 'unknown;; (logior platform-os-darwin platform-cpu-arm)                
		:p2-dispatch #(unknown) ;; not referenced - can't use 'unknown because of tyep decl on slot
		:p2-vinsn-templates (make-hash-table) ;; not referenced - can't use 'unknown because of tyep decl on slot
		:p2-template-hash-name 'unknown ;;'*arm-vinsn-templates*
		:p2-compile 'ev2-compile
		:target-specific-features
		'(:cvm :cvm-target :darwin-target :darwincvm-target
                       ;; Who wants to know about endianness?
                       :64-bit-target :little-endian-target)
		:target-fasl-pathname (make-pathname :type "cvmfsl")
		:target-platform (logior platform-word-size-64
                                         platform-cpu-cvm
                                         platform-os-darwin)
		:target-os :darwincvm
		:name :darwincvm
		:target-arch-name :cvm
		:target-foreign-type-data  *cvm-ftd*
                :target-arch cvm::*cvm-target-arch*))




;;;;;
;;;; Ok so thisi is what #$SEEK_CHAR
#|
%load-var (name)
there is *target-ftd* which someone set up somewhere....
(do-interface-dirs (d) => FTD-DIRLIST is a double linked list, starts out empty
so somewhere it gets grown.

does some computing to get the TYPE,  so then can do
(%cons-foreign-variable string type)
and (resolve-foreign-variable )

So make a fake dir and always find stuff there.
   have db-vars which is apparently a 'CDB'
   (cdb-get vars <key macptr> <value macptr>)
   vartype (extract-db-type <value macptr> ftd)

Ok, so we're compiling some file for TARGET.  In the file there are read macros
like #$FOO

The thing running in our lisp has definitions of these read macros, but will try to
lookup symbols in the *TARGET-FTD*.

|#   



(let ((old (member (backend-name *darwincvm-backend*) *known-backends* :key #'backend-name)))
  (when old (format t "~&Updating ~s" (backend-name *darwincvm-backend*)))
  (if old
    (setf (car old) *darwincvm-backend*)
    (push *darwincvm-backend* *known-backends*)))

(defparameter *cvm-backend* *darwincvm-backend*)

;;;#+cvm-target
;;;(setq *host-backend* *cvm-backend* *target-backend* *host-backend*)


          
#|
(defun setup-arm-ftd (backend)
  (or (backend-target-foreign-type-data backend)
      (let* ((name (backend-name backend))
             (ftd
              (case name
                (:darwinarm
                 (make-ftd :interface-db-directory "ccl:darwin-arm-headers;"
			   :interface-package-name "ARM-DARWIN"
                           :attributes '(:bits-per-word  32
                                         :signed-char t
                                         :struct-by-value t
                                         :natural-alignment t
                                         :prepend-underscore nil)
                           :ff-call-expand-function
                           (intern "EXPAND-FF-CALL" "ARM-DARWIN")
			   :ff-call-struct-return-by-implicit-arg-function
                           (intern "RECORD-TYPE-RETURNS-STRUCTURE-AS-FIRST-ARG"
                                   "ARM-DARWIN")
                           :callback-bindings-function
                           (intern "GENERATE-CALLBACK-BINDINGS" "ARM-DARWIN")
                           :callback-return-value-function
                           (intern "GENERATE-CALLBACK-RETURN-VALUE" "ARM-DARWIN")))
                (:linuxarm
                 (make-ftd :interface-db-directory "ccl:arm-headers;"
			   :interface-package-name "ARM-LINUX"
                           :attributes '(:bits-per-word  32
                                         :signed-char nil
                                         :natural-alignment t
                                         :struct-by-value t)
                           :ff-call-expand-function
                           (intern "EXPAND-FF-CALL" "ARM-LINUX")
			   :ff-call-struct-return-by-implicit-arg-function
                           (intern "RECORD-TYPE-RETURNS-STRUCTURE-AS-FIRST-ARG"
                                   "ARM-LINUX")
                           :callback-bindings-function
                           (intern "GENERATE-CALLBACK-BINDINGS" "ARM-LINUX")
                           :callback-return-value-function
                           (intern "GENERATE-CALLBACK-RETURN-VALUE" "ARM-LINUX")))
                (:androidarm
                 (make-ftd :interface-db-directory "ccl:android-headers;"
			   :interface-package-name "ARM-ANDROID"
                           :attributes '(:bits-per-word  32
                                         :signed-char nil
                                         :natural-alignment t
                                         :struct-by-value t)
                           :ff-call-expand-function
                           (intern "EXPAND-FF-CALL" "ARM-ANDROID")
			   :ff-call-struct-return-by-implicit-arg-function
                           (intern "RECORD-TYPE-RETURNS-STRUCTURE-AS-FIRST-ARG"
                                   "ARM-LINUX")
                           :callback-bindings-function
                           (intern "GENERATE-CALLBACK-BINDINGS" "ARM-ANDROID")
                           :callback-return-value-function
                           (intern "GENERATE-CALLBACK-RETURN-VALUE" "ARM-ANDROID"))))))
        (install-standard-foreign-types ftd)
        (use-interface-dir :libc ftd)
        (setf (backend-target-foreign-type-data backend) ftd))))



(defmacro make-fake-stack-frame (sp next-sp fn lr vsp xp)
  `(ccl::%istruct 'arm::fake-stack-frame ,sp ,next-sp ,fn ,lr ,vsp ,xp))

(defun arm::eabi-record-type-returns-structure-as-first-arg (rtype)
  (when (and rtype
             (not (typep rtype 'unsigned-byte))
             (not (member rtype *foreign-representation-type-keywords*
                          :test #'eq)))
    (let* ((ftype (if (typep rtype 'foreign-type)
                    rtype
                    (parse-foreign-type rtype))))
      (when (typep ftype 'foreign-record-type)
        (ensure-foreign-type-bits ftype)
        (> (foreign-type-bits ftype) 32)))))

(defun arm::eabi-expand-ff-call (callform args &key (arg-coerce #'null-coerce-foreign-arg) (result-coerce #'null-coerce-foreign-result))
  (let* ((result-type-spec (or (car (last args)) :void))
         (enclosing-form nil)
         (result-form nil))
    (multiple-value-bind (result-type error)
        (ignore-errors (parse-foreign-type result-type-spec))
      (if error
        (setq result-type-spec :void result-type *void-foreign-type*)
        (setq args (butlast args)))
      (collect ((argforms))
        (when (typep result-type 'foreign-record-type)
          (setq result-form (pop args))
          (if (arm::eabi-record-type-returns-structure-as-first-arg result-type)
            (progn
              (setq result-type *void-foreign-type*
                    result-type-spec :void)
              (argforms :address)
              (argforms result-form))
            ;; This only happens in the SVR4 ABI.
            (progn
              (setq result-type (parse-foreign-type :unsigned-doubleword)
                    result-type-spec :unsigned-doubleword
                    enclosing-form `(setf (%%get-unsigned-longlong ,result-form 0))))))
        (unless (evenp (length args))
          (error "~s should be an even-length list of alternating foreign types and values" args))        
        (do* ((args args (cddr args)))
             ((null args))
          (let* ((arg-type-spec (car args))
                 (arg-value-form (cadr args)))
            (if (or (member arg-type-spec *foreign-representation-type-keywords*
                           :test #'eq)
                    (typep arg-type-spec 'unsigned-byte))
              (progn
                (argforms arg-type-spec)
                (argforms arg-value-form))
              (let* ((ftype (parse-foreign-type arg-type-spec)))
                (if (typep ftype 'foreign-record-type)
                  (progn
                    (argforms :address)
                    (argforms arg-value-form))
                  (progn
                    (argforms (foreign-type-to-representation-type ftype))
                    (argforms (funcall arg-coerce arg-type-spec arg-value-form))))))))
        (argforms (foreign-type-to-representation-type result-type))
        (let* ((call (funcall result-coerce result-type-spec `(,@callform ,@(argforms)))))
          (if enclosing-form
            `(,@enclosing-form ,call)
            call))))))

(defun arm::eabi-generate-float-callback-bindings (stack-ptr  argvars argspecs result-spec struct-result-name)
  (collect ((lets)
            (rlets)
            (dynamic-extent-names))
    (let* ((hard-float-p (gensym)))
      (lets `(,hard-float-p (arm-hard-float-p)))
      (let* ((rtype (parse-foreign-type result-spec)))
        (when (typep rtype 'foreign-record-type)
          (let* ((bits (ensure-foreign-type-bits rtype)))
            (if (<= bits 64)
              (rlets (list struct-result-name (foreign-record-type-name rtype)))
              (setq argvars (cons struct-result-name argvars)
                    argspecs (cons :address argspecs)
                    rtype *void-foreign-type*))))
        (let* ((reg-offset 0)
               (gen-offset 0)
               (fp-offset -72)
               (stack-offset 16))
          (do* ((argvars argvars (cdr argvars))
                (argspecs argspecs (cdr argspecs)))
               ((null argvars)
                (values (rlets) (lets) (dynamic-extent-names) nil rtype nil 0 #|wrong|#))
            (let* ((name (car argvars))
                   (spec (car argspecs))
                   (argtype (parse-foreign-type spec))
                   (accessor nil)
                   (offsetform nil))
              (if (typep argtype 'foreign-record-type)
                (setq argtype (parse-foreign-type :address)))
              (typecase argtype
                (foreign-single-float-type
                 (setq accessor '%get-single-float
                       offsetform `(if ,hard-float-p
                                    ,(if (< fp-offset -8)
                                         (prog1 fp-offset
                                           (incf fp-offset 4))
                                         (prog1 stack-offset
                                           (incf stack-offset 4)))
                                    ,(prog1 gen-offset
                                            (incf gen-offset 4)))))
                (foreign-double-float-type
                 (when (logtest 7 fp-offset)
                   (incf fp-offset 4))
                 (when (and (>= fp-offset -8)
                            (logtest 7 stack-offset))
                   (incf stack-offset 4))
                 (when (logtest 7 gen-offset)
                   (incf gen-offset 4))
                 (setq accessor '%get-double-float
                       offsetform `(if ,hard-float-p
                                    ,(if (< fp-offset -8)
                                         (prog1 fp-offset
                                           (incf fp-offset 8))
                                         (prog1 stack-offset
                                           (incf stack-offset 8)))
                                    ,(prog1 gen-offset
                                            (incf gen-offset 8)))))
                (foreign-pointer-type
                 (setq accessor '%get-ptr
                       offsetform `(if ,hard-float-p
                                    ,(if (< reg-offset 16)
                                         (prog1 reg-offset
                                           (incf reg-offset 4))
                                         (prog1 stack-offset
                                           (incf stack-offset 4)))
                                    ,(prog1 gen-offset
                                            (incf gen-offset 4)))))
                (foreign-integer-type
                 (let* ((nbits (foreign-type-bits argtype))
                        (nbytes (cond ((> nbits 32) 8)
                                      ((> nbits 16) 4)
                                      ((> nbits 8) 2)
                                      (t 1)))
                        (align (if (= nbytes 8) 8 4))
                        (signed (foreign-integer-type-signed argtype)))
                   (when (= align 8)
                     (when (logtest 7 reg-offset)
                       (incf reg-offset 4))
                     (when (and (>= reg-offset 16)
                                (logtest 7 stack-offset))
                       (incf stack-offset 4))
                     (when (logtest 7 gen-offset)
                       (incf gen-offset 4)))
                   (setq accessor
                         (case nbytes
                           (8 (if signed '%%get-signed-longlong '%%get-unsigned-longlong))
                           (4 (if signed '%get-signed-long '%get-unsigned-long))
                           (2 (if signed '%get-signed-word '%get-unsigned-word))
                           (1 (if signed '%get-signed-byte '%get-unsigned-byte)))
                         offsetform `(if ,hard-float-p
                                      ,(if (<= (+ reg-offset nbytes) 16)
                                           (prog1 reg-offset
                                             (incf reg-offset nbytes))
                                           (prog1 stack-offset
                                             (incf stack-offset nbytes)))
                                      ,(prog1 gen-offset
                                              (incf gen-offset nbytes)))))))
              (when name (lets `(,name (,accessor ,stack-ptr ,offsetform)))))))))))

(defun arm::eabi-generate-callback-bindings (stack-ptr fp-args-ptr argvars argspecs result-spec struct-result-name)
  (declare (ignore fp-args-ptr))
  (if (dolist (argtype argspecs)
        (let* ((ftype (parse-foreign-type argtype)))
          (when (or (typep ftype 'foreign-single-float-type)
                    (typep ftype 'foreign-double-float-type))
            (return t))))
    (arm::eabi-generate-float-callback-bindings stack-ptr argvars argspecs result-spec struct-result-name)
    (collect ((lets)
              (rlets)
              (dynamic-extent-names))
      (let* ((rtype (parse-foreign-type result-spec)))
        (when (typep rtype 'foreign-record-type)
          (let* ((bits (ensure-foreign-type-bits rtype)))
            (if (<= bits 64)
              (rlets (list struct-result-name (foreign-record-type-name rtype)))
              (setq argvars (cons struct-result-name argvars)
                    argspecs (cons :address argspecs)
                    rtype *void-foreign-type*))))
        (let* ((offset 0)
               (nextoffset offset))
          (do* ((argvars argvars (cdr argvars))
                (argspecs argspecs (cdr argspecs)))
               ((null argvars)
                (values (rlets) (lets) (dynamic-extent-names) nil rtype nil 0 #|wrong|#))
            (let* ((name (car argvars))
                   (spec (car argspecs))
                   (argtype (parse-foreign-type spec)))
              (if (typep argtype 'foreign-record-type)
                (setq argtype (parse-foreign-type :address)))
              (let* ((access-form
                      `(,(cond
                          ((typep argtype 'foreign-single-float-type)
                           (setq nextoffset (+ offset 4))
                           '%get-single-float)
                          ((typep argtype 'foreign-double-float-type)
                           (when (logtest offset 4)
                             (incf offset 4))
                           (setq nextoffset (+ offset 8))
                           '%get-double-float)
                          ((and (typep argtype 'foreign-integer-type)
                                (= (foreign-integer-type-bits argtype) 64)
                                (foreign-integer-type-signed argtype))
                           (when (logtest offset 4)
                             (incf offset 4))
                           (setq nextoffset (+ offset 8))
                           '%%get-signed-longlong)
                          ((and (typep argtype 'foreign-integer-type)
                                (= (foreign-integer-type-bits argtype) 64)
                                (not (foreign-integer-type-signed argtype)))
                           (when (logtest offset 4)
                             (incf offset 4))
                           (setq nextoffset (+ offset 8))
                           '%%get-unsigned-longlong)
                          (t
                           (setq nextoffset (+ offset 4))
                           (cond ((typep argtype 'foreign-pointer-type) '%get-ptr)
                                 ((typep argtype 'foreign-integer-type)
                                  (let* ((bits (foreign-integer-type-bits argtype))
                                         (signed (foreign-integer-type-signed argtype)))
                                    (cond ((<= bits 8)
                                           (if signed
                                             '%get-signed-byte
                                             '%get-unsigned-byte))
                                          ((<= bits 16)
                                           (if signed
                                             '%get-signed-word 
                                             '%get-unsigned-word))
                                          ((<= bits 32)
                                           (if signed
                                             '%get-signed-long 
                                             '%get-unsigned-long))
                                          (t
                                           (error "Don't know how to access foreign argument of type ~s" (unparse-foreign-type argtype))))))
                                 (t
                                  (error "Don't know how to access foreign argument of type ~s" (unparse-foreign-type argtype))))))
                        ,stack-ptr
                        ,offset)))
                (when name (lets (list name access-form)))
                (setq offset nextoffset)))))))))

(defun arm::eabi-generate-callback-return-value (stack-ptr fp-args-ptr result return-type struct-return-arg)
  (declare (ignore fp-args-ptr))
  (unless (eq return-type *void-foreign-type*)
    (let* ((return-type-keyword
            (if (typep return-type 'foreign-record-type)
              (progn
                (setq result `(%%get-unsigned-longlong ,struct-return-arg 0))
                :unsigned-doubleword)
              (foreign-type-to-representation-type return-type)))
           (offset -8))
      `(setf (,
              (case return-type-keyword
                (:address '%get-ptr)
                (:signed-doubleword '%%get-signed-longlong)
                (:unsigned-doubleword '%%get-unsigned-longlong)
                (:double-float '%get-double-float)
                (:single-float '%get-single-float)
                (:unsigned-fullword '%get-unsigned-long)
                (t '%get-long)) ,stack-ptr ,offset) ,result))))

#+arm-target
(require "ARM-VINSNS")
|#



	      


  
