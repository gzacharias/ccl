;;;-*- Mode: Lisp; Package: (CVM :use CL) -*-
;;;
;;; Copyright 2010 Clozure Associates
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

(defpackage "CVM"
  (:use "CL")
  #+cvm-target
  (:nicknames "TARGET"))


(require "ARCH")

(in-package "CVM")

(defconstant fulltag-even-fixnum 0)
(defconstant fulltag-single-float 1)
(defconstant fulltag-character 2)
(defconstant fulltag-cons 3)
(defconstant fulltag-nil 11)
(defconstant fulltag-immediate 4) ;; was tra-0, reuse it...
(defconstant fulltag-odd-fixnum 8)
;; 12 is available (was tra-1)
(defconstant fulltag-misc 13)
(defconstant fulltag-symbol 14)
(defconstant fulltag-function 15)

(defconstant lisptag-fixnum 0)
(defconstant lisptag-single-float 1)
(defconstant lisptag-character 2)
(defconstant lisptag-list 3)
(defconstant lisptag-immediate 4)
(defconstant lisptag-misc 5)
(defconstant lisptag-symbol 6)
(defconstant lisptag-function 7)

;; Pass 1 of the compiler assumes this, so we have no choice.  See *nx-64-bit-fixnum-type*
(defconstant num-fixnum-bits 61) ;; don't really have a choice


;; Define all the subtags from x86, but we won't be using them all!

(defconstant gvector-subtags-0 5)       ;; reserved for use in subtags
(defconstant gvector-subtags-1 6)       ;; reserved for use in subtags
(defconstant ivector-subtags-misc 7)    ;; reserved for use in subtags
(defconstant ivector-subtags-32-bit 9)  ;; reserved for use in subtags
(defconstant ivector-subtags-64-bit 10) ;; reserved for use in subtags

(defparameter *cvm-uvector-subtags* nil)
(defparameter *cvm-gvector-types* nil)

(defmacro define-subtags (code &rest names)
  `(progn
     ,@(loop for index = #x10 then (+ index #x10)
         for spec in names
         as name = (if (consp spec) (car spec) spec)
         as key = (let ((pname (string name)))
                    (assert (string= "SUBTAG-" pname :end2 (length "SUBTAG-")))
                    (intern (subseq pname (length "SUBTAG-")) :keyword))
         do (when (consp spec)
              (let ((new-index (ash (cadr spec) 4)))
                (assert (<= index new-index))
                (setq index new-index)))
         do (assert (<= index #xF00))
         collect `(defconstant ,name (+ ,code ,index))
         collect `(push (cons ,key ,name) *cvm-uvector-subtags*)
         if (member code '(gvector-subtags-0 gvector-subtags-1))
         collect `(push ,key *cvm-gvector-types*))))

;; Array-typecode-p assumes arrayh is the lowest CL array gvector

(define-subtags gvector-subtags-0
  subtag-symbol
  subtag-catch-frame
  subtag-hash-vector
  subtag-pool
  subtag-population
  subtag-package
  subtag-slot-vector
  subtag-basic-stream
  subtag-function
  subtag-call-frame
  (subtag-array-header 11))

(define-subtags gvector-subtags-1
  subtag-ratio
  subtag-complex
  subtag-struct
  subtag-istruct
  subtag-value-cell
  subtag-xfunction
  subtag-lock
  subtag-instance
  subtag-lexpr-vector
  (subtag-vector-header 11)
  subtag-simple-vector)

(define-subtags ivector-subtags-misc
  (subtag-complex-double-float-vector 9)
  subtag-signed-16-bit-vector  ;; 'word-vector
  subtag-unsigned-16-bit-vector  ;; 'unsigned-word-vector
  (subtag-signed-8-bit-vector 13) ; 'byte-vector
  subtag-unsigned-8-bit-vector ;;unsigned-byte-vector
  subtag-bit-vector)  ;; bit-vector

(defconstant min-cl-ivector-subtag  subtag-complex-double-float-vector)

(push (cons :min-cl-ivector-subtag  min-cl-ivector-subtag) *cvm-uvector-subtags*)

;(defconstant min-8-bit-ivector-subtag subtag-s8-vector)
;(defconstant max-8-bit-ivector-subtag subtag-u8-vector)

(define-subtags ivector-subtags-32-bit
  subtag-bignum
  subtag-double-float
  subtag-xcode-vector
  subtag-complex-single-float
  subtag-complex-double-float
  (subtag-simple-string 12)
  subtag-signed-32-bit-vector
  subtag-unsigned-32-bit-vector
  subtag-single-float-vector)

(define-subtags ivector-subtags-64-bit
  subtag-macptr
  subtag-dead-macptr
  (subtag-complex-single-float-vector 11)
  subtag-fixnum-vector
  subtag-signed-64-bit-vector
  subtag-unsigned-64-bit-vector
  subtag-double-float-vector)

;;; Variables we have no intention of defining, any uses will have to be fixed if they actually
;;; come up at runtime.   Don't need compiler warnings
(declaim (special area.code 
                  area.gc-count 
                  area.high 
                  area.low 
                  area.older 
                  area.softlimit 
                  area.succ 
                  area.threshold 
                  area.younger 
                  catch-frame.catch-tag-cell 
                  catch-frame.db-link 
                  catch-frame.link 
                  catch-frame.link-cell 
                  complex-double-float.realpart 
                  complex-single-float.realpart 
                  cons.car 
                  cons.cdr 
                  cons.size 
                  double-float.value-cell 
                  misc-complex-dfloat-offset 
                  misc-dfloat-offset 
                  tag-misc 
                  tcr-bias 
                  tcr.activate 
                  tcr.catch-top 
                  tcr.cs-area 
                  tcr.db-link 
                  tcr.flags 
                  tcr.interrupt-pending 
                  tcr.log2-allocation-quantum 
                  tcr.native-thread-id 
                  tcr.osid 
                  tcr.reset-completion 
                  tcr.suspend-count 
                  tcr.ts-area 
                  tcr.vs-area 
                  tcr.xframe 
                  value-cell-header 
                  value-cell.value-cell))

(defun %kernel-global (sym)
  (error "Who is calling ~s on ~s" '%kernel-global sym))

;;; And these variables just get directly referenced all over the place
(defconstant nbits-in-word 64)
(defconstant num-subtag-bits 8) ;; heh, like you really could change this!
(defconstant fixnumshift 3)
(defconstant fixnum-shift fixnumshift)

(defconstant word-shift 3)

(defconstant subtag-single-float fulltag-single-float)

(defconstant tag-fixnum lisptag-fixnum)
(defconstant tag-list lisptag-list)

(defconstant subtag-weak subtag-population)

(defconstant arg-check-trap-pc-limit 1)

;;; find whoever is using these, make sure it's not doing arithmetic on objects
(defconstant misc-data-offset (- 8 fulltag-misc))
(defconstant symbol.vcell 2) ;; as opposed to symbol.vcell-cell, which can be used with uvref

(defconstant target-most-positive-fixnum (1- (ash 1 (1- num-fixnum-bits))))
(defconstant target-most-negative-fixnum  (- (ash 1 (1- num-fixnum-bits))))

(defconstant arrayH.rank-cell 0)
(defconstant arrayH.physsize-cell 1)
(defconstant arrayH.data-vector-cell 2)
(defconstant arrayH.displacement-cell 3)
(defconstant arrayH.flags-cell 4)
(defconstant arrayH.dim0-cell 5)

;(defconstant arrayH.flags-cell-bits-byte (byte 8 0))
(defconstant arrayH.flags-cell-subtag-byte (byte 8 8))

(defconstant vectorH.logsize-cell 0)
(defconstant vectorH.physsize-cell 1)
(defconstant vectorH.data-vector-cell 2)
(defconstant vectorH.displacement-cell 3)
(defconstant vectorH.flags-cell 4)

;(defconstant lock._value-cell 0)
(defconstant lock.kind-cell 1)
;(defconstant lock.writer-cell 2)
;(defconstant lock.name-cell 3)
;(defconstant lock.whostate-cell 4)
;(defconstant lock.whostate-2-cell 5)

(defconstant subtag-simple-base-string subtag-simple-string)
(defconstant subtag-vectorh subtag-vector-header)
(defconstant subtag-arrayh subtag-array-header)
(defconstant subtag-character fulltag-character)
;; setup-ioblock-input
(defconstant subtag-u8-vector subtag-unsigned-8-bit-vector)
(defconstant subtag-s8-vector subtag-signed-8-bit-vector)
(defconstant subtag-u16-vector subtag-unsigned-16-bit-vector)
(defconstant subtag-s16-vector subtag-signed-16-bit-vector)
(defconstant subtag-u32-vector subtag-unsigned-32-bit-vector)
(defconstant subtag-s32-vector subtag-signed-32-bit-vector)
(defconstant subtag-u64-vector subtag-unsigned-64-bit-vector)
(defconstant subtag-s64-vector subtag-signed-64-bit-vector)


(defconstant node-size 8) ;; see array-rank-limit


(ccl::defenum (:start 0 :step 8)
  lockptr.avail
  lockptr.owner
  lockptr.count
  lockptr.signal
  lockptr.waiting
  lockptr.malloced-ptr-NOT-REFERENCED
  lockptr.spinlock)
(defconstant lockptr.size 56)

(ccl::defenum (:start 0 :step 8)
  rwlock.spin
  rwlock.state
  rwlock.blocked-writers
  rwlock.blocked-readers
  rwlock.writer
  rwlock.reader-signal
  rwlock.writer-signal
  rwlock.malloced-ptr-NOT-REFERENCED)
(defconstant rwlock.size 64)

(defconstant value-cell.value-cell 0)

(defmacro def-uvector-object (name &rest slots)
  `(progn
     (ccl::defenum ()
                   ,@(mapcar #'(lambda (slot) (ccl::form-symbol name "." slot "-CELL"))
                             slots))
     (defconstant ,(ccl::form-symbol name ".ELEMENT-COUNT") ,(length slots))))

(def-uvector-object macptr
  address
  domain
  type
 )

(def-uvector-object xmacptr
  address
  domain
  type
  flags
  link
)

(def-uvector-object lock
  _value                                ;finalizable pointer to kernel object
  kind                                  ; '0 = recursive-lock, '1 = rwlock
  writer				;tcr of owning thread or 0
  name
  whostate
  whostate-2
  )

(def-uvector-object symbol
  pname
  vcell
  fcell
  package-predicate
  flags
  plist
  binding-index
)

(def-uvector-object ratio
  numer
  denom)

(def-uvector-object complex ()
  realpart
  imagpart
)

(defconstant double-float.val-low-cell 0)
(defconstant double-float.val-high-cell 1)
;(defconstant double-float.element-count 2)


(defun cvm-array-type-name-from-ctype (ctype)
  (when (typep ctype 'ccl::array-ctype)
    (let* ((element-type (ccl::array-ctype-element-type ctype)))
      (typecase element-type
        (ccl::class-ctype
         (let* ((class (ccl::class-ctype-class element-type)))
           (if (or (eq class ccl::*character-class*)
                   (eq class ccl::*base-char-class*)
                   (eq class ccl::*standard-char-class*))
             :simple-string
             :simple-vector)))
        (ccl::numeric-ctype
         (if (eq (ccl::numeric-ctype-complexp element-type) :complex)
           (case (ccl::numeric-ctype-format element-type)
             (single-float :complex-single-float-vector)
             (double-float :complex-double-float-vector)
             (t :simple-vector))
           (case (ccl::numeric-ctype-class element-type)
             (integer
              (let* ((low (ccl::numeric-ctype-low element-type))
                     (high (ccl::numeric-ctype-high element-type)))
                (cond ((or (null low) (null high))
                       :simple-vector)
                      ((and (>= low 0) (<= high 1))
                       :bit-vector)
                      ((and (>= low 0) (<= high 255))
                       :unsigned-8-bit-vector)
                      ((and (>= low 0) (<= high 65535))
                       :unsigned-16-bit-vector)
                      ((and (>= low 0) (<= high #xffffffff))
                       :unsigned-32-bit-vector)
                      ((and (>= low 0) (<= high #xffffffffffffffff))
                       :unsigned-64-bit-vector)
                      ((and (>= low -128) (<= high 127))
                       :signed-8-bit-vector)
                      ((and (>= low -32768) (<= high 32767))
                       :signed-16-bit-vector)
                      ((and (>= low (ash -1 31)) (<= high (1- (ash 1 31))))
                       :signed-32-bit-vector)
                      ((and (>= low target-most-negative-fixnum)
                            (<= high target-most-positive-fixnum))
                       :fixnum-vector)
                      ((and (>= low (ash -1 63)) (<= high (1- (ash 1 63))))
                       :signed-64-bit-vector)
                      (t :simple-vector))))
             (float
              (case (ccl::numeric-ctype-format element-type)
                ((double-float long-float) :double-float-vector)
                ((single-float short-float) :single-float-vector)
                (t :simple-vector)))
             (t :simple-vector))))
        (ccl::unknown-ctype)
        (ccl::named-ctype
         (if (eq element-type ccl::*universal-type*)
           :simple-vector))
        (t nil)))))


(defparameter *cvm-target-arch*
  (arch::make-target-arch :name :cvm
                          :lisp-node-size 'arch::unknown
                          :nil-value 'arch::unknown
                          :fixnum-shift 'arch::unknown
                          :most-positive-fixnum target-most-positive-fixnum
                          :most-negative-fixnum target-most-negative-fixnum
                          :misc-data-offset 'arch::unknown
                          :misc-dfloat-offset 'arch::unknown
                          :nbits-in-word nbits-in-word
                          :ntagbits 4
                          :nlisptagbits 3
                          :uvector-subtags *cvm-uvector-subtags*
                          :max-64-bit-constant-index 'arch::unknown;;max-64-bit-constant-index
                          :max-32-bit-constant-index 'arch::unknown ;;max-32-bit-constant-index
                          :max-16-bit-constant-index 'arch::unknown ;;max-16-bit-constant-index
                          :max-8-bit-constant-index 'arch::unknown ;; max-8-bit-constant-index
                          :max-1-bit-constant-index 'arch::unknown ;; max-1-bit-constant-index
                          :word-shift  'arch::unknown ;;2
                          :code-vector-prefix  'arch::unknown ;;()
                          ;; x8664 seems to not include :basic-stream and xfunction , maybe not on purpose?
                          :gvector-types (set-difference *cvm-gvector-types* '(:array-header :vector-header))
                          :1-bit-ivector-types 'arch::unknown ;;'(:bit-vector)
                          :8-bit-ivector-types 'arch::unknown ;;'(:signed-8-bit-vector :unsigned-8-bit-vector)
                          :16-bit-ivector-types 'arch::unknown ;;'(:signed-16-bit-vector :unsigned-16-bit-vector)
                          :32-bit-ivector-types  'arch::unknown #+not-yet'(:signed-32-bit-vector
                                                  :unsigned-32-bit-vector
                                                  :single-float-vector
                                                  :fixnum-vector
                                                  :single-float
                                                  :double-float
                                                  :bignum
                                                  :simple-string)
                          :64-bit-ivector-types 'arch::unknown ;'(:double-float-vector :complex-single-float-vector)
                          :array-type-name-from-ctype-function  #'cvm-array-type-name-from-ctype
                          :package-name "CVM"
                          :t-offset 'arch::unknown ;;t-offset
                          :array-data-size-function 'arch::unknown;; #'arm-misc-byte-count
                          :fpr-mask-function 'arch::unknown;; 'arm-fpr-mask
                          :subprims-base 'arch::unknown;; arm::*arm-subprims-base*
                          :subprims-shift 'arch::unknown;; arm::*arm-subprims-shift*
                          :subprims-table 'arch::unknown;; arm::*arm-subprims*
                          :primitive->subprims  'arch::unknown ;; `(((0 . 23) . ,(ccl::%subprim-name->offset '.SPbuiltin-plus arm::*arm-subprims*)))
                          :unbound-marker-value 'arch::unknown;; unbound-marker
                          :slot-unbound-marker-value 'arch::unknown;; slot-unbound-marker
                          :fulltagmask  #b1111
                          :fixnum-tag 0
                          :single-float-tag fulltag-single-float
                          ;; This determines whether compiler uses typecode (if true) or fulltag (if false)
                          :single-float-tag-is-subtag nil
                          :double-float-tag subtag-double-float 
                          :cons-tag fulltag-cons
                          :null-tag fulltag-nil
                          :symbol-tag subtag-symbol
                          :symbol-tag-is-subtag t
                          :function-tag  subtag-function
                          :function-tag-is-subtag t
                          :subtag-char fulltag-character
                          :fulltag-misc fulltag-misc ;;  x8664::fulltag-misc
                          ;; This is accessed in file compiler but only used in dumping, so no actually used by us.
                          :big-endian nil
                          :misc-subtag-offset  'arch::unknown;;misc-subtag-offset
                          :car-offset  'arch::unknown;;cons.car
                          :cdr-offset  'arch::unknown ;;cons.cdr
                          :charcode-shift 'arch::unknown ;;charcode-shift
                          :char-code-limit #x110000
                          ))

;;; arch macros
(defmacro def-cvm-archmacro (name lambda-list &body body)
  `(arch::defarchmacro :cvm ,name ,lambda-list ,@body))


;; Double float layout
(def-cvm-archmacro ccl::%make-dfloat () `(ccl::%alloc-misc 2 ,subtag-double-float))

(def-cvm-archmacro ccl::%numerator (x) `(ccl::%svref ,x 0))
(def-cvm-archmacro ccl::%denominator (x) `(ccl::%svref ,x 1))

(def-cvm-archmacro ccl::%realpart (x) `(ccl::uvref ,x 0))
(def-cvm-archmacro ccl::%imagpart (x) `(ccl::uvref ,x 1))

(def-cvm-archmacro ccl::immediate-p-macro (thing)
  (let* ((tag (gensym)))
    `(let* ((,tag (ccl::lisptag ,thing)))
       (declare (type (unsigned-byte 3) ,tag))
       (logbitp ,tag ,(logior (ash 1 lisptag-fixnum)
                              (ash 1 lisptag-single-float)
                              (ash 1 lisptag-character)
                              (ash 1 lisptag-immediate))))))

(def-cvm-archmacro ccl::hashed-by-identity (thing)
  (let* ((typecode (gensym)))
    `(let* ((,typecode (ccl::typecode ,thing)))
      (declare (fixnum ,typecode))
      (or (= ,typecode subtag-instance)
          (and (<= ,typecode 7)
               (logbitp ,typecode ,(logior (ash 1 lisptag-fixnum)
                                           (ash 1 lisptag-single-float)
                                           (ash 1 lisptag-character)
                                           (ash 1 lisptag-immediate)
                                           (ash 1 lisptag-symbol))))))))


;; We don't care about the tag, so...
(def-cvm-archmacro ccl::function-to-function-vector (f) f)
(def-cvm-archmacro ccl::function-vector-to-function (v) v)
(def-cvm-archmacro ccl::lfun-vector (f) f)
(def-cvm-archmacro ccl::lfun-vector-lfun (v) v)

;; Not having this errs at compile time, so..
(def-cvm-archmacro ccl::area-code () 'area.code)
(def-cvm-archmacro ccl::area-succ () 'area.succ)

(def-cvm-archmacro ccl::nth-immediate (f i)
  `(ccl::%nth-immediate ,f (the fixnum (- (the fixnum ,i) 1))))

(def-cvm-archmacro ccl::set-nth-immediate (f i new)
  `(ccl::%set-nth-immediate ,f (the fixnum (- (the fixnum ,i) 1)) ,new))

;; The lap functions %symbol->symptr and %symptr->symbol is where we
;; handle nilsym <-> nil.  These are just tag manipulations
(def-cvm-archmacro ccl::symptr->symvector (s) s)
(def-cvm-archmacro ccl::symvector->symptr (s) s)

(def-cvm-archmacro ccl::%get-kernel-global (name)
  ;; 'weak-gc-method  'batch-flag 'all-areas 'tenured-area 'statically-linked 'host-platform 'batch-flag
  ;; static-cons-area free-static-conses ret1valaddr (unquoted!)
  ;; 'ppc::altivec-present 'stack-size 'default-allocation-quantum 'oldest-ephemeral
  (when (ccl::quoted-form-p name) (setq name (cadr name)))
  `(ccl::cvm-get-kernel-global ',name))

;; The assumption that this is a macptr is baked deeply into the code.  Will have to arrange for that to be true
(def-cvm-archmacro ccl::%get-kernel-global-ptr (name macptr)
  (when (ccl::quoted-form-p name) (setq name (cadr name)))
  `(ccl::cvm-get-kernel-global-ptr ',name ,macptr))

;; This gets looked up by the fasldumper, but we won't actually get that far.  But have it for now
(defconstant fasl-version #x66)
(defconstant fasl-max-version #x66)
(defconstant fasl-min-version #x66)
;(defparameter *image-abi-version* 1045)





(provide "CVM-ARCH")
