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

#+cvm-target (next-nx-defops)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "BACKEND"))

(eval-when (:compile-toplevel :execute)
  (require "NXENV")
  ;; This would be lib;cmvenv.lisp, but there is nothing we want to put there.
  ;(require "CVMENV")
  )


(defparameter *known-cvm-backends* nil)

(defmacro def-known-backend (var backend)
  `(progn
     (defparameter ,var ,backend)
     (push ,var *known-cvm-backends*)
     (let ((old (member (backend-name ,var) *known-backends* :key #'backend-name)))
       (cond (old
              (format t "~&Updating ~s" (backend-name ,var))
              (setf (backend-target-foreign-type-data ,var)
                    (backend-target-foreign-type-data (car old)))
              (setf (car old) ,var))
             (t
              (push ,var *known-backends*))))
     ',var))

#+(or darwincvm-target (not cvm-target))
(def-known-backend *darwincvm-backend*
  (make-backend :lookup-opcode 'unknown ;;#'arm::lookup-arm-instruction
                :lookup-macro 'unknown ;; #'false
                :lap-opcodes #() ;; not referrenced - can't use 'unknown because of type decl on slot
                :define-vinsn 'unknown ;;'%define-arm-vinsn
                :platform-syscall-mask 'unknown;; (logior platform-os-darwin platform-cpu-arm)                
                :p2-dispatch #(unknown) ;; not referenced - can't use 'unknown because of tyep decl on slot
                :p2-vinsn-templates (make-hash-table) ;; not referenced - can't use 'unknown because of tyep decl on slot
                :p2-template-hash-name 'unknown ;;'*arm-vinsn-templates*
                :p2-compile 'cvm2-compile
                :target-specific-features
                '(:cvm :cvm-target :darwin-target :darwincvm-target
                       ;; Who wants to know about endianness?
                       :64-bit-target :little-endian-target)
                :target-fasl-pathname (make-pathname :type "bc")
                :target-platform (logior platform-word-size-64
                                         platform-cpu-cvm
                                         platform-os-darwin)
                :target-os :darwincvm
                :name :darwincvm
                :target-arch-name :cvm
                :target-arch cvm::*cvm-target-arch*))

(defparameter *cvm-backend* (car *known-cvm-backends*))

#+cvm-target
(setq *host-backend* *cvm-backend* *target-backend* *host-backend*)


(defvar *cvm-ftd*)

(defun make-cvm-ftd ()
  (let ((ftd (make-ftd :interface-db-directory 'unknown  ;;"ccl:darwin-cvm-headers;"
                       :interface-package-name "CVMDARWIN-FFI"
                       :attributes (list :bits-per-word 64
                                         :signed-char t
                                         :struct-by-value t
                                         :prepend-underscores nil
                                         :defer-to-runtime t)
                       :ff-call-expand-function 'cvm-expand-ff-call
                       :ff-call-struct-return-by-implicit-arg-function 'unknown ;; (Intern ...)
                       :callback-bindings-function 'unknown ;; (intern ..
                       :callback-return-value-function 'unknown;; (intern
                       )))
    ;; This might not be necessary any more.  TODO: TRY WITHOUT.
    ;;  With arrays, it might be trying to get the size at compile time... check it out
    ;; called twice, (:array (:struct :pollfd) 1) and (:array :int)
    (setf (gethash :array (ftd-translators ftd))
          (lambda (whole env)
            (declare (ignorable whole env))
            (FORMAT  T "~&**** TRANSLATING ~s" whole)
            (make-foreign-array-type :element-type (make-foreign-pointer-type))))
    (setq *cvm-ftd* ftd)))

;; When running in CVM, make-cvm-ftd is called from foreign-types (once make-ftd is defined),
;;  and stored in *host-backend* (and install-standard-foreign-types is then called on it from l1-boot-2
;;  after that's defined).
;;
;; When running in another host, this file loads before foreign-types, so make-ftd and
;; install-standard-foreign-types are not defined yet.  l1-boot-2 calls SETUP-CVM-FTD after loading foreign-types.
;; (If this file is reloaded into a running lisp, def-known-backend carries the old ftd over.)
#-cvm-target
(defun setup-cvm-ftd ()
  (let ((ftd (make-cvm-ftd)))
    (install-standard-foreign-types ftd)
    (setf (backend-target-foreign-type-data *cvm-backend*) ftd)))

;; l1-aprims does this:
;(defpackage #.(ftd-interface-package-name (backend-target-foreign-type-data *target-backend*))
;  (:nicknames "OS")
;  (:use "COMMON-LISP"))
; BUT compile-file does not bind *target-backend* at read time, only compile-named-function binds it.
; which means at load time, it defines the package that was host when it was compiled.

(eval-when (:execute :load-toplevel) ;; doing this at compile time interferes with cross compilation package manipulations
  (or (find-package "CVMDARWIN-FFI")
      (make-package "CVMDARWIN-FFI" :use "COMMON-LISP")))

;; To be defined in the host.  ***TODO: CHECK THIS ONCE IN A WHILE
(declaim (ftype function
                cvm-symbolp
                cvm-ivector-typecode-p
                cvm-gvectorp

                cvm-make-combined-method
                cvm-make-gf
                cvm-make-writer-method
                cvm-make-reader-method
                cvm-make-gf
                cvm-make-slot-getter
                cvm-make-slot-setter
                cvm-make-slot-lookup-fn
                cvm-make-type-fn

                cvm-%kernel-import

                cvm-external-call
                cvm-access-foreign-field
                setf-cvm-access-foreign-field
                cvm-access-foreign-array
                cvm-os-constant
                (setf cvm-access-foreign-array)
                cvm-foreign-bit-size
                cvm-foreign-field-byte-offset
                cvm-get-kernel-global
                (setf cvm-get-kernel-global)
                cvm-get-kernel-global-ptr
                cvm-xdisassemble

                make-bclambda-lfun
                lfun-bclambda))


;; foreign-type-to-repesentation-type gets into pasing and databases and stuff we don't support.
;; Just kludge it.
(defparameter *foreign-type-to-representation-type*
  '((:int . :signed-fullword)
    (:signed . :signed-fullword)
    (:unsigned . :unsigned-fullword)
    (:ssize_t . :signed-doubleword)
    (:off_t . :signed-doubleword)
    (:mode_t . :unsigned-halfword)
    (:socklen_t . :unsigned-fullword)))

;; x8664::expand-ff-call
(defun cvm-expand-ff-call (callform args)
  (let ((ffn (cadr callform)))
    (when (and (consp ffn) (eq (car ffn) '%kernel-import))
      ;; Need to pass in the name of the kernel fn not the offset.  TODO: just define all the kernel constants,
      ;; like (defconstant cvm::kernel-import-lisp-opendir 'cvm::kernel-import-lisp-opendir) etc. and leave this alone.
      (destructuring-bind (offset) (cdr ffn)
        (assert (symbolp offset))
        (setq callform `(,(car callform) (cvm-%kernel-import ',offset) ,@(cddr callform))))))
  (flet ((std (type-spec)
           ;; Don't want to call foreign-type-to-representation-type because that gets into parsing and databases and stuff...
           ; (foreign-type-to-representation-type type-spec)
           (if (or (member type-spec *foreign-representation-type-keywords* :test #'eq)
                   (typep type-spec 'unsigned-byte))
             type-spec
             (or (cdr (assoc type-spec *foreign-type-to-representation-type*))
                 (error "Unknown type spec ~s" type-spec)))))
    `(,@callform ,@(loop while (cdr args) collect (std (pop args)) collect (pop args))
                 ,(if args (std (car args)) (std :void)))))

;; Don't want to redefine ccl function, so..  For debugging only.
(define-compiler-macro %kernel-import (&whole call offset)
  (when (eq *target-backend* *cvm-backend*)
    (break "who still calls this? ~s" offset))
  call)

(provide "CVM-BACKEND")
