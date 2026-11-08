;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; The CVM's counterpart of xfasload: building level-0 for the VM.
;;;
;;; For the native targets, xload-level-0 compiles level-0 and then loads the fasls into a
;;; heap image.  For the cvm target, its "fasls" (i.e. the .bc files) *are* the heap image,
;;; so xload-level-0 only needs to compile them.
;;;
;;; In the VM this is loaded by the xload-level-0 stub in compile-ccl.
;;; In other hosts it is loaded explicitly tools/xdarwincvm.lisp.

(in-package "CCL")

(defun cvm-compile-level-0 ()
  ;; TODO: Maybe it's time to make a *level-0-files* so don't rely on contents of directories..
  (assert (eq *target-backend* *cvm-backend*))
  (let* ((*ccl-system*
          (loop for dir in '("ccl:level-0;" "ccl:level-0;CVM;")
            nconc (loop for src in (sort (directory (merge-pathnames dir "*.lisp")) #'string< :key #'namestring)
                    collect (list (intern (string-upcase (pathname-name src)) :ccl)
                                  (make-pathname :type nil :defaults src)
                                  src)))))
    (target-compile-modules (mapcar #'car *ccl-system*) (backend-name *target-backend*) nil)))

#+cvm-target
(defun xload-level-0 (&optional force)
  (declare (ignore force))
  (cvm-compile-level-0))

;; The counterpart of writing the heap image: collect the .bc files into a directory
(defun collect-all-fasls (&optional (dest "ccl:ccl-bc;"))
  (let ((*.fasl-pathname* (backend-target-fasl-pathname *cvm-backend*)))
    ;; Delete dest before get fasls so that don't try to copy the fasls from dest
    (recursive-delete-directory dest :if-does-not-exist nil)
    (copy-all-fasls "ccl:" dest)))

(defun restore-all-fasls (&optional (src "ccl:ccl-bc;"))
  (let ((*.fasl-pathname* (backend-target-fasl-pathname *cvm-backend*)))
    (copy-all-fasls src "ccl:" :if-exists :supersede)))

(defun copy-all-fasls (srcdir destdir &key (if-exists :error))
  (let* ((fasls-path (full-pathname (merge-pathnames (merge-pathnames "**/*" srcdir) *.fasl-pathname*)))
         (ndirs (1- (length (pathname-directory fasls-path))))
         (seen-dirs ()))
    (loop for file in (directory fasls-path :follow-links nil)
      as subdirs = (nthcdr ndirs (pathname-directory file))
      as destsubdir = (merge-pathnames (make-pathname :directory `(:relative ,@subdirs)) destdir)
      do (unless (member subdirs seen-dirs :test 'equal)
           (push subdirs seen-dirs)
           (ensure-directories-exist destsubdir))
      do (copy-file file (merge-pathnames destsubdir file) :if-exists if-exists))))

(provide "XCVMFASLOAD")
