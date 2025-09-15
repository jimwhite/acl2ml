;;;; acl2-batch-generate.lisp
;;;; Batch script using saved_acl2 to properly read ACL2 books

(format t "ACL2ML Batch Generation with saved_acl2~%")
(format t "==========================================~%")

;; Create ACL2 package so we can read ACL2 books
(unless (find-package "ACL2")
  (defpackage "ACL2" (:use :common-lisp)))

;; Load our extraction system
(load "extraction.lisp")
(load "table-to-feature-vector.lisp")

;; Use the extraction package
(in-package :acl2ml-complete-original)

(defun find-certified-acl2-books (directory)
  "Find all certified ACL2 books (those with .cert files)"
  (let ((cert-files nil))
    (let ((cmd (format nil "find ~A -name '*.cert' -type f" directory)))
      (with-input-from-string (stream
        (with-output-to-string (out)
          (sb-ext:run-program "/bin/sh" (list "-c" cmd) :output out :wait t)))
        (loop for line = (read-line stream nil)
              while line
              when (> (length line) 0)
              do (let ((lisp-file (concatenate 'string (subseq line 0 (- (length line) 5)) ".lisp")))
                   (when (probe-file lisp-file)
                     (push lisp-file cert-files))))))
    (nreverse cert-files)))

(defun safe-filename (path)
  "Convert file path to safe filename for definitions"
  (substitute #\_ #\/ (substitute #\_ #\. path)))

;; Enhanced error-tolerant extraction
(defun extract-with-acl2-packages (file-path)
  "Extract from ACL2 file with proper package handling"
  (handler-case
      (progn
        ;; Make sure ACL2 package exists
        (unless (find-package "ACL2")
          (defpackage "ACL2" (:use :common-lisp)))

        ;; Try our extraction with package safety
        (let ((*package* (find-package "ACL2")))
          (export-library-original-format file-path)))
    (error (e)
      (format t "  → Package error, trying Common Lisp package: ~A~%" e)
      (handler-case
          (let ((*package* (find-package "COMMON-LISP-USER")))
            (export-library-original-format file-path))
        (error (e2)
          (format t "  → Final error: ~A~%" e2)
          nil)))))

(defun batch-generate-with-packages (&optional (books-dir "/home/acl2/books"))
  "Generate definitions handling ACL2 package issues"
  (format t "Processing certified books with ACL2 package support...~%")

  (let ((books (find-certified-acl2-books books-dir))
        (processed 0)
        (successful 0)
        (package-errors 0))

    (format t "Found ~A certified ACL2 books~%" (length books))
    (ensure-directories-exist "definitions/global/")

    (dolist (book books)
      (let* ((safe-name (safe-filename book))
             (def-file (format nil "definitions/global/~A" safe-name)))

        (format t "[~A/~A] Processing ~A...~%" (1+ processed) (length books) book)

        (let ((definitions (extract-with-acl2-packages book)))
          (if definitions
              (progn
                (with-open-file (stream def-file
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
                  (format stream "(~{~A~^ ~})~%" definitions))
                (format t "  → Generated ~A definitions~%" (length definitions))
                (incf successful))
              (progn
                (format t "  → FAILED~%")
                (incf package-errors)))
          (incf processed))

        ;; Progress report every 50 books
        (when (zerop (mod processed 50))
          (format t "~%PROGRESS: ~A/~A processed, ~A successful, ~A failed~%~%"
                  processed (length books) successful package-errors))))

    (format t "~%BATCH GENERATION COMPLETE~%")
    (format t "=========================~%")
    (format t "Processed: ~A books~%" processed)
    (format t "Successful: ~A books~%" successful)
    (format t "Failed: ~A books~%" package-errors)
    (format t "Success rate: ~,1F%~%" (* 100.0 (/ successful processed)))

    successful))

;; Run the batch generation
(batch-generate-with-packages "/home/acl2/books")

;; Exit
(sb-ext:exit)