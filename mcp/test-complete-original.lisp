;;;; Test the complete original extraction
:q
(load "~/quicklisp/setup.lisp")
(load "/workspaces/acl2ml/mcp/complete-original-extraction.lisp")
(acl2ml-complete-original:test-complete-original-extraction)

;; Test with real file
(format t "~%Testing with example.lisp:~%")
(let ((result (acl2ml-complete-original:export-library-original-format "/workspaces/acl2ml/manual/example.lisp")))
  (if result
      (progn
        (format t "SUCCESS: Generated ~A definitions~%" (length result))
        (format t "First definition: ~A~%" (first result))
        (format t "Second definition: ~A~%" (second result)))
      (format t "FAILED: No definitions generated~%")))