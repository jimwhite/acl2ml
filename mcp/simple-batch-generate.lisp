;;;; simple-batch-generate.lisp
;;;; Simple batch script to generate definitions for some ACL2 books

(format t "ACL2ML Simple Batch Generation~%")
(format t "===============================~%")

;; Load our extraction system
(load "extraction.lisp")
(load "table-to-feature-vector.lisp")

;; Use the extraction package
(in-package :acl2ml-complete-original)

;; List of interesting books to process
(defparameter *books-to-process*
  '("/home/acl2/books/arithmetic-3/bind-free/basic.lisp"
    "/home/acl2/books/arithmetic-3/bind-free/common.lisp"
    "/home/acl2/books/arithmetic-3/pass1/basic-arithmetic.lisp"
    "/home/acl2/books/arithmetic-5/top.lisp"
    "/home/acl2/books/std/lists/append.lisp"
    "/home/acl2/books/std/lists/len.lisp"))

;; Ensure global definitions directory exists
(ensure-directories-exist "definitions/global/")

(format t "Processing ~A books...~%~%" (length *books-to-process*))

(let ((processed 0)
      (successful 0))

  (dolist (book *books-to-process*)
    (let* ((safe-name (substitute #\_ #\/ (substitute #\_ #\. book)))
           (def-file (format nil "definitions/global/~A" safe-name)))

      (format t "[~A/~A] Processing ~A...~%"
              (1+ processed) (length *books-to-process*) book)

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

(format t "~%BATCH GENERATION COMPLETE~%")
(format t "=========================~%")
(format t "Processed: ~A books~%" processed)
(format t "Successful: ~A books~%" successful)

(sb-ext:exit)