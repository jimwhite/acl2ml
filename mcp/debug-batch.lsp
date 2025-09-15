:q
; Debug version with detailed logging
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")
(use-package :acl2ml-complete-original)

(defun test-single-book ()
  "Test processing a single book with detailed output"
  (let ((book "/home/acl2/books/arithmetic-2/pass1/basic-arithmetic.lisp"))
    (format t "Testing book: ~A~%" book)

    ; Test file reading
    (format t "1. Testing file exists: ~A~%" (probe-file book))

    ; Test form reading
    (format t "2. Testing form reading...~%")
    (let ((forms nil)
          (error-occurred nil))
      (handler-case
          (with-open-file (stream book :direction :input)
            (let ((count 0))
              (loop for form = (read stream nil :eof)
                    until (eq form :eof)
                    do (progn
                         (push form forms)
                         (incf count)
                         (when (= (mod count 10) 0)
                           (format t "  Read ~A forms so far...~%" count))))
              (setf forms (reverse forms))
              (format t "  Successfully read ~A forms~%" (length forms))))
        (error (e)
          (format t "  ERROR reading forms: ~A~%" e)
          (setf error-occurred t)))

      (unless error-occurred
        ; Test extraction
        (format t "3. Testing extraction...~%")
        (handler-case
            (let ((definitions (extract-acl2-definitions-original-pipeline forms book)))
              (format t "  Successfully extracted ~A definitions~%"
                      (if definitions (length definitions) 0))
              (when (and definitions (> (length definitions) 0))
                (format t "  First definition: ~S~%" (car definitions))))
          (error (e)
            (format t "  ERROR in extraction: ~A~%" e)))))))

(test-single-book)
(quit)