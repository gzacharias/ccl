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
              ;(format t "~&Updating ~s" (backend-name ,var))
              (setf (backend-target-foreign-type-data ,var)
                    (backend-target-foreign-type-data (car old)))
              (setf (car old) ,var))
             (t
              (push ,var *known-backends*))))
     ',var))

(defun make-cvm-backend (name os-features platform-os)
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
                `(:cvm :cvm-target ,@os-features)
                :target-fasl-pathname (make-pathname :type "bc")
                :target-platform (logior platform-word-size-64
                                         platform-cpu-cvm
                                         platform-os)
                :target-os name
                :name name
                :target-arch-name :cvm
                :target-arch cvm::*cvm-target-arch*))

#+(or darwincvm-target (not cvm-target))
(def-known-backend *darwincvm-backend*
  (make-cvm-backend :darwincvm
                    '(:darwin-target :darwincvm-target
                      ;; Who wants to know about endianness?
                      :64-bit-target :little-endian-target
                      :unix :darwin)
                    platform-os-darwin))

#+(or linuxcvm-target (not cvm-target))
(def-known-backend *linuxcvm-backend*
  (make-cvm-backend :linuxcvm
                    '(:linux-target :linuxcvm-target :64-bit-target :little-endian-target :unix :linux)
                    platform-os-linux))

;; The VM target for the OS we're running on.
(defparameter *cvm-backend*
  (find #+linux-target :linuxcvm #-linux-target :darwincvm *known-cvm-backends* :key #'backend-name))

(defun cvm-ffi-package-name (backend)
  (ecase (backend-name backend)
    (:darwincvm "CVMDARWIN-FFI")
    (:linuxcvm "CVMLINUX-FFI")))

#+cvm-target
(setq *host-backend* *cvm-backend* *target-backend* *host-backend*)


(defvar *cvm-ftd*)

(defun make-cvm-ftd (&optional (backend *cvm-backend*))
  (let ((ftd (make-ftd :interface-db-directory 'unknown  ;;"ccl:darwin-cvm-headers;"
                       :interface-package-name (cvm-ffi-package-name backend)
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
;; In another host, LOAD-CVM-BACKEND (tools/xdarwincvm.lisp) loads this file and then calls SETUP-CVM-FTD.
;; (If this file is reloaded into a running lisp, def-known-backend carries the old ftd over.)
#-cvm-target
(defun setup-cvm-ftd (backend)
  (let ((ftd (make-cvm-ftd backend)))
    (install-standard-foreign-types ftd)
    (setf (backend-target-foreign-type-data backend) ftd)))

;; l1-aprims does this:
;(defpackage #.(ftd-interface-package-name (backend-target-foreign-type-data *target-backend*))
;  (:nicknames "OS")
;  (:use "COMMON-LISP"))
; BUT compile-file does not bind *target-backend* at read time, only compile-named-function binds it.
; which means at load time, it defines the package that was host when it was compiled.

(eval-when (:execute :load-toplevel) ;; doing this at compile time interferes with cross compilation package manipulations
  (dolist (backend *known-cvm-backends*)
    (let ((name (cvm-ffi-package-name backend)))
      (or (find-package name)
          (make-package name :use "COMMON-LISP")))))

;; To be defined in the host.  Check this once in a while, see https://github.com/gzacharias/ccl/issues/3
(declaim (ftype function
                cvm-symbolp
                cvm-ivector-typecode-p
                cvm-gvectorp

                cvm-make-combined-method
                cvm-make-gf
                cvm-make-writer-method
                cvm-make-reader-method
                cvm-make-slot-getter
                cvm-make-slot-setter
                cvm-make-slot-lookup-fn
                cvm-make-type-fn

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

                cvm-run-program

                make-bclambda-lfun
                lfun-bclambda
                map-bclambda-immediates))

;; Lap functions in ccl-vm (runtime.lisp); their lisp definitions are #-cvm-target.  Without
;; this, bc-compile-ccl warns about them on hosts that don't have them natively (e.g. a linux ccl).
(declaim (ftype function soname-from-mach-header))

;; This is never actually called (cvm2 turns calls to it into $bc-kernel-call), but
;; cvm-expand-ff-call generates the calls, so it's needed to avoid warnings.
(declaim (ftype function cvm-%kernel-import))


;; foreign-type-to-repesentation-type gets into pasing and databases and stuff we don't support.
;; Just kludge it.
(defparameter *foreign-type-to-representation-type*
  '((:int . :signed-fullword)
    (:signed . :signed-fullword)
    (:unsigned . :unsigned-fullword)
    (:size_t . :unsigned-doubleword)
    (:ssize_t . :signed-doubleword)
    (:off_t . :signed-doubleword)
    (:mode_t . :unsigned-halfword)
    (:socklen_t . :unsigned-fullword)
    (:pthread_t . :unsigned-doubleword)))

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
           (when (and type-spec (symbolp type-spec) (not (keywordp type-spec)))
             (setq type-spec (intern (symbol-name type-spec) :keyword)))
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
