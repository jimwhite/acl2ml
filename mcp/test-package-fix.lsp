:q
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")
(use-package :acl2ml-complete-original)

; Switch to ACL2 package before reading
(in-package "ACL2")

(defun test-basic-arithmetic ()
  "Test the failing basic-arithmetic.lisp file"
  (let ((book "/home/acl2/books/arithmetic-2/pass1/basic-arithmetic.lisp"))
    (format t "Testing ~A with ACL2 package~%" book)

    (with-open-file (stream book :direction :input)
      (let ((forms nil))
        (loop for form = (read stream nil :eof)
              until (eq form :eof)
              do (push form forms))
        (setf forms (reverse forms))
        (format t "Read ~A forms~%" (length forms))

        ; Test first DEFTHM
        (let ((first-defthm (find-if (lambda (form)
                                       (and (listp form)
                                            (equal (car form) 'defthm)))
                                     forms)))
          (when first-defthm
            (format t "First DEFTHM: ~S~%" first-defthm)

            ; Test extract-info
            (handler-case
                (let ((info (extract-info first-defthm)))
                  (format t "Extract-info SUCCESS: ~S~%" info))
              (error (e)
                (format t "Extract-info ERROR: ~A~%" e)))

            ; Test full pipeline
            (handler-case
                (let ((result (extract-acl2-definitions-original-pipeline forms book)))
                  (format t "Pipeline result: ~A definitions~%"
                          (if result (length result) 0)))
              (error (e)
                (format t "Pipeline ERROR: ~A~%" e)))))))))

(test-basic-arithmetic)
(quit)