;;;; test-certified-batch.lisp
;;;; Test the certified books batch generation on a small subset

(format t "Testing Certified Books Batch Generation~%")
(format t "=========================================~%")

;; Load our extraction system
(load "extraction.lisp")
(load "table-to-feature-vector.lisp")

;; Use the extraction package
(in-package :acl2ml-complete-original)

(defun find-certified-acl2-books-limited (directory limit)
  "Find limited number of certified ACL2 books for testing"
  (let ((cert-files nil))
    (let ((cmd (format nil "find ~A -name '*.cert' -type f | head -~A" directory limit)))
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

;; Test with 10 certified books
(let ((books (find-certified-acl2-books-limited "/home/acl2/books" 10))
      (processed 0)
      (successful 0))

  (format t "Found ~A certified books for testing~%" (length books))
  (ensure-directories-exist "definitions/global/")

  (dolist (book books)
    (let* ((safe-name (substitute #\_ #\/ (substitute #\_ #\. book)))
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
              (format t "  → Generated ~A definitions~%" (length definitions))
              (incf successful))
            (incf processed))
        (error (e)
          (format t "  → ERROR: ~A~%" e)
          (incf processed)))))

(format t "~%TEST COMPLETE - Ready to run on all certified books!~%")

(sb-ext:exit)