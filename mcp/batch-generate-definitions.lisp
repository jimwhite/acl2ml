;;;; batch-generate-definitions.lisp
;;;; Batch script to generate definitions for all ACL2 certified books

(format t "ACL2ML Batch Definitions Generation~%")
(format t "====================================~%")

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

(defun batch-generate-global-definitions (&optional (books-dir "/home/acl2/books"))
  "Generate global definitions for ALL certified ACL2 books"
  (format t "Scanning for certified ACL2 books in ~A...~%" books-dir)

  (let ((books (find-certified-acl2-books books-dir))
        (processed 0)
        (successful 0))

    (format t "Found ~A certified ACL2 books~%" (length books))
    (format t "Processing ALL certified books...~%~%" )

    ;; Ensure global definitions directory exists
    (ensure-directories-exist "definitions/global/")

    (dolist (book books)
      (let* ((safe-name (safe-filename book))
             (def-file (format nil "definitions/global/~A" safe-name)))

        (format t "[~A/~A] Processing ~A...~%" (1+ processed) (length books) book)

        (handler-case
            (let ((definitions (export-library-original-format book)))
              (when definitions
                (with-open-file (stream def-file
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
                  (format stream "(~{~A~^ ~})~%" definitions))
                (format t "  → Generated ~A definitions in ~A~%" (length definitions) def-file)
                (incf successful))
              (incf processed))
          (error (e)
            ;; Log error to file instead of console
            (with-open-file (error-log "batch-errors.log"
                                       :direction :output
                                       :if-exists :append
                                       :if-does-not-exist :create)
              (format error-log "[~A] ERROR in ~A: ~A~%"
                      (get-universal-time) book e))
            (format t "  → ERROR (logged)~%")
            (incf processed))))

    (format t "~%BATCH GENERATION COMPLETE~%")
    (format t "=========================~%")
    (format t "Processed: ~A books~%" processed)
    (format t "Successful: ~A books~%" successful)
    (format t "Failed: ~A books~%" (- processed successful))
    (format t "Success rate: ~,1F%~%" (* 100.0 (/ successful processed)))
    (format t "Generated files in definitions/global/~%")
    (format t "Error log written to batch-errors.log~%")

    successful)))

;; Run the batch generation for ALL certified books
(batch-generate-global-definitions "/home/acl2/books")

;; Exit
(sb-ext:exit)