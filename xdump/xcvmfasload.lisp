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

;; The counterpart of writing the heap image.  if DEST is a directory, all the bc files are placed there.
;;  Otherwise, it will be a .tar.gz file containing a toplevel directory with the bc files.
(defun write-cvm-bundle (dest)
  (setq dest (full-pathname dest)) ;; so don't have to deal with versions in namestrings!
  (if (directory-pathname-p dest)
    (collect-all-fasls dest)
    (let* ((name (file-namestring dest))
           (end (- (length name) (length ".tar.gz"))))
      (when (and (> end 0) (string= name ".tar.gz" :start1 end))
        (setq name (%substr name 0 end)))
      (setq dest (make-pathname :name name :type "tar.gz" :defaults dest))
      (let* ((destdir (make-pathname :name nil :type nil :defaults dest))
             (tmpdir (merge-pathnames (make-pathname :directory `(:relative ".ccl-bc-tmp")) destdir))
             (outdir (merge-pathnames (make-pathname :directory `(:relative ,name)) tmpdir))
             (tmpfile (make-pathname :type "tmp" :defaults dest)))
        (unwind-protect
            (progn
              (collect-all-fasls outdir)
              (let ((proc (run-program "tar" (list "-czf" (native-translated-namestring tmpfile)
                                                   "-C" (native-translated-namestring tmpdir)
                                                   name)
                                       ;; No AppleDouble ._ entries from macOS tar
                                       :env '(("COPYFILE_DISABLE" . "1"))
                                       :output t :error :output)))
                (multiple-value-bind (status exit-code) (external-process-status proc)
                  (unless (and (eq status :exited) (eql exit-code 0))
                    (error "tar failed (~s ~s) writing ~s" status exit-code tmpfile)))))
          (recursive-delete-directory tmpdir :if-does-not-exist nil))
        (rename-file tmpfile dest :if-exists :supersede))))
  (format t "~&;Wrote bc bundle: ~s" (truename dest)))

(defun collect-all-fasls (dest)
  (assert (directory-pathname-p dest))
  (let ((true-dest (probe-file dest)))
    (when true-dest (assert (not (equalp true-dest (truename "ccl:"))))))
  (let ((*.fasl-pathname* (backend-target-fasl-pathname *cvm-backend*)))
    ;; Delete dest before get fasls so that don't try to copy the fasls from dest
    (recursive-delete-directory dest :if-does-not-exist nil)
    (copy-all-fasls "ccl:" dest)))

(defun restore-all-fasls (src)
  (let ((*.fasl-pathname* (backend-target-fasl-pathname *cvm-backend*)))
    (copy-all-fasls src "ccl:" :if-exists :supersede)))

(defun copy-all-fasls (srcdir destdir &key (if-exists :error))
  (assert (directory-pathname-p srcdir))
  (assert (directory-pathname-p destdir))
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
