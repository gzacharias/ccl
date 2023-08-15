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

(next-nx-defops)

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
                :target-fasl-pathname (make-pathname :type "cvmsrc")
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
    ;; L1-init calls (%foreign-type-or-record-size :timeval :bytes) At READ time. Kludge it,
    ;;  makes sure to give it enough bits for any reasonable :timeval structure...
    ;;; TODO: ** I think this is fixed now.
    ;; HOPEFULLY won't need this because make-foreign-record-type is not defined  yet.
    #+try-without (setf (gethash :timeval (ftd-struct-definitions ftd))
                        (make-foreign-record-type :kind :struct
                                                  :name :timeval
                                                  :bits 1024))
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
;; When running in another host, assume this file is loaded into a fully initialized
;; lisp, so make-ftd and install-standard-foreign-types are defined.
#-cvm-target (setf (backend-target-foreign-type-data *cvm-backend*)
                   (let ((ftd (make-cvm-ftd)))
                     (install-standard-foreign-types ftd) ;; l1-boot-2 calls this after loading foreign-types
                     ftd))
#-cvm-target (format t "~&have set ftd for ~s" *cvm-backend*)

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
                cvm-ivectorp
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

;; DOn't want to redefine ccl function, so..  For debugging only.
(define-compiler-macro %kernel-import (offset)
  (when (eq *target-backend* *cvm-backend*)
    (break "who still calls this? ~s" offset)))

(provide "CVM-BACKEND")
