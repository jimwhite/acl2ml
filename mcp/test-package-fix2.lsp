:q
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")
(use-package :acl2ml-complete-original)

(defun test-package-fix ()
  "Test reading in ACL2 package then processing in our package"
  (let ((book "/home/acl2/books/arithmetic-2/pass1/basic-arithmetic.lisp"))
    (format t "Testing package fix for ~A~%" book)

    ; Read forms in ACL2 package
    (format t "1. Reading forms in ACL2 package...~%")
    (let ((forms nil)
          (*package* (find-package "ACL2")))
      (with-open-file (stream book :direction :input)
        (loop for form = (read stream nil :eof)
              until (eq form :eof)
              do (push form forms)))
      (setf forms (reverse forms))
      (format t "   Read ~A forms~%" (length forms))

      ; Show first DEFTHM and what package its symbols are in
      (let ((first-defthm (find-if (lambda (form)
                                     (and (listp form)
                                          (equal (car form) 'defthm)))
                                   forms)))
        (when first-defthm
          (format t "   First DEFTHM: ~S~%" first-defthm)
          (format t "   DEFTHM symbol package: ~A~%"
                  (package-name (symbol-package (car first-defthm))))
          (format t "   EQUAL symbol package: ~A~%"
                  (package-name (symbol-package 'equal)))))

      ; Process with our extraction functions (staying in current package)
      (format t "2. Processing with extraction functions...~%")
      (handler-case
          (let ((definitions (extract-acl2-definitions-original-pipeline forms book)))
            (format t "   SUCCESS: ~A definitions extracted~%"
                    (if definitions (length definitions) 0)))
        (error (e)
          (format t "   ERROR: ~A~%" e))))))

(test-package-fix)
(quit)