;;; Cross-compile setup for any target, when the host is the CVM (ccl-vm).
;;;
;;; The target's setup file (e.g. tools/xdarwinx8664.lisp) assumes a native CCL host.
;;; This file supplies what ccl-vm does differently.  Use:
;;;
;;;     (load "ccl:tools;cvm-xcompile.lisp")
;;;     (cvm-load-cross-setup "ccl:tools;xdarwinx8664.lisp")
;;;
;;; and then cross-compile as that file says.

(in-package "CCL")

;;; xfasload reads the cross-compiled fasls with CCL's native fasl reader (%FASLOAD from
;;; level-0/nfasload.lisp, with its own dispatch table).  ccl-vm doesn't load nfasload: it
;;; implements %FASLOAD (for .bc files) and nfasload's package/symbol-table functions
;;; itself.  So evaluate only the parts of nfasload.lisp that the VM doesn't provide, and
;;; make %FASLOAD with a dispatch table argument use nfasload's version.
(defun toplevel-defined-function-names (form)
  (when (consp form)
    (case (car form)
      ((defun %fhave) (list (let* ((name (cadr form)))
                              (if (and (consp name) (eq (car name) 'quote)) (cadr name) name))))
      ((progn let let* eval-when) (mapcan #'toplevel-defined-function-names (cddr form))))))

(defun load-native-fasload ()
  (let* ((vm-fasload #'%fasload)
         (*package* (find-package "CCL"))
         (eof (list nil))
         (skipped ()))
    (with-open-file (stream "ccl:level-0;nfasload.lisp")
      (loop for form = (read stream nil eof)
            until (eq form eof)
            do (let* ((names (toplevel-defined-function-names form)))
                 (cond ((equal names '(%fasload))
                        (eval `(defun %native-fasload ,@(cddr form))))
                       ((some #'fboundp names)
                        (push names skipped))
                       ((and (consp form) (member (car form) '(setq setf defconstant))
                             (symbolp (cadr form)) (boundp (cadr form))
                             (symbol-value (cadr form)))
                        (push (cadr form) skipped))
                       ((and (consp form) (eq (car form) 'defvar) (eq (cadr form) '%toplevel-function%))
                        (push (cadr form) skipped))
                       (t (eval form))))))
    (format t "~&;; nfasload: kept the VM's definitions of ~s~%" (reverse skipped))
    (%fhave '%fasload
            #'(lambda (string &optional (table nil table-p))
                (if table-p
                  (%native-fasload string table)
                  (funcall vm-fasload string))))))

;;; The bundle's db-io leaves out the interface-database lookups that only a target with an
;;; interface database needs (#-CVM-TARGET); a cross-compiler for a real target needs them.
(defun load-cross-db-io ()
  (let* ((*package* (find-package "CCL"))
         (eof (list nil))
         (wanted '(interface-constant-value interface-var-type interface-function)))
    (with-open-file (stream "ccl:lib;db-io.lisp")
      (let* ((*features* (remove :cvm-target *features*)))
        ;; Stop when they've been found: later forms use #. on compile-time-only constants.
        (loop with missing = (copy-list wanted)
              for form = (if missing (read stream nil eof) eof)
              until (eq form eof)
              do (when (and (consp form) (eq (car form) 'defun) (member (cadr form) missing))
                   (setq missing (remove (cadr form) missing))
                   (unless (fboundp (cadr form))
                     (eval form))))))
    (dolist (name wanted)
      (unless (fboundp name)
        (error "Didn't find ~s in db-io.lisp" name)))))

;;; Package hash tables.  xload-save-htab copies a package's (htvec count . limit) into
;;; the image, where the target's FIND-SYMBOL will probe it with its own hash function.
;;; But the VM's packages aren't laid out that way (the VM keeps the symbols unhashed and
;;; looks them up in a host hash table), so lay them out as the target CCL would, using
;;; the target's hash: %PNAME-HASH, MIXUP-HASH-CODE on target fixnums (whose arithmetic
;;; wraps at (speed 3) (safety 0)) and FAST-MOD (an unsigned divide of the tagged words).
;;; Only checked against a native x8664 image.
(defun target-wrap-fixnum (x target-max-fixnum)
  (let* ((modulus (* 2 (1+ target-max-fixnum))))
    (- (mod (+ x (1+ target-max-fixnum)) modulus) (1+ target-max-fixnum))))

(defun target-hash-pname (pname target-max-fixnum)
  (flet ((wrap (x) (target-wrap-fixnum x target-max-fixnum)))
    (let* ((hash 0))
      (dotimes (i (length pname))
        (setq hash (logxor (logior (logand (ash hash 5) #xffffffff) (ash hash -27))
                           (char-code (schar pname i)))))
      (let* ((fixnum (logand hash target-max-fixnum))
             (mixed 0))
        (loop until (zerop fixnum)
              do (setq mixed (wrap (+ mixed (logand fixnum #xff)))
                       fixnum (ash fixnum -8)
                       mixed (wrap (+ mixed (wrap (ash mixed 10))))
                       mixed (logxor mixed (ash mixed -6))))
        (setq mixed (wrap (+ mixed (wrap (ash mixed 3))))
              mixed (logxor mixed (ash mixed -11))
              mixed (wrap (+ mixed (wrap (ash mixed 15)))))
        (values mixed (svref #(5 7 11 13 17 19 23 29) (logand mixed 7)))))))

(defun target-htab-size (nsyms)
  ;; As %RESIZE-HTAB and %INITIALIZE-HTAB in level-0/nfasload.lisp.
  (let* ((size (+ nsyms (ash nsyms -2) 2)))
    (if (> size 32749)
      (do* ((next (logior 1 size) (+ next 2)))
           ((every (lambda (p) (eql 1 (gcd next p))) '(5 7 11 13 17 19 23 29)) next))
      (find-if (lambda (p) (>= p size))
               '(41 61 97 149 223 337 509 769 887 971 1153 1559 1733
                 2609 2801 3917 5879 8819 13229 19843 24989 29789 32749)))))

(defun target-layout-htab (htab target-max-fixnum)
  (let* ((symbols (loop for s across (car htab) when (symbolp s) collect s))
         (size (target-htab-size (length symbols)))
         (vector (make-array size :initial-element 0))
         (word-modulus (* 2 (1+ target-max-fixnum))))
    (dolist (sym symbols)
      (multiple-value-bind (primary secondary)
          (target-hash-pname (symbol-name sym) target-max-fixnum)
        (do* ((i (mod (mod primary word-modulus) size)
                 (let* ((next (+ i secondary))) (if (>= next size) (- next size) next))))
             ((eql (svref vector i) 0) (setf (svref vector i) sym)))))
    (cons vector (cons (length symbols) (- size (ash size -3))))))

(defun install-target-htab-layout ()
  ;; CROSS-XLOAD-LEVEL-0 binds *TARGET-BACKEND* to the target's backend.
  (let* ((save-htab #'xload-save-htab))
    (%fhave 'xload-save-htab
            #'(lambda (htab)
                (funcall save-htab
                         (target-layout-htab
                          htab
                          (arch::target-most-positive-fixnum
                           (backend-target-arch *target-backend*))))))))

(defun cvm-load-cross-setup (target-setup-file)
  ;; xfasload puts LOCAL-VC-REVISION (git/svn describe, via RUN-PROGRAM) in the image's
  ;; version string.  The VM can't run programs, so the image's version won't include it.
  (%fhave 'local-vc-revision #'(lambda () nil))
  (load-native-fasload)
  (load-cross-db-io)
  (load target-setup-file)
  ;; The target setup file loads xfasload.
  (install-target-htab-layout))
