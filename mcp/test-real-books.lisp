;;;; Test script to generate global definitions from real ACL2 books
:q
(load "~/quicklisp/setup.lisp")
(load "/workspaces/acl2ml/mcp/library-export.lisp")

;; Generate definitions from real ACL2 books
(in-package :acl2ml-library-export)

;; Test with actual ACL2 book files
(let ((test-books '("/home/acl2/books/arithmetic-3/bind-free/basic.lisp"
                   "/home/acl2/books/arithmetic-3/bind-free/common.lisp")))

  (format t "~%TESTING REAL ACL2 BOOKS PROCESSING~%")
  (format t "==================================~%")

  (dolist (book-file test-books)
    (format t "~%Processing: ~A~%" book-file)
    (when (probe-file book-file)
      (let* ((safe-name (substitute-if #\_ (lambda (c) (member c '(#\/ #\.)))
                                      (enough-namestring book-file "/home/acl2/books/")))
             (global-def-file (format nil "/workspaces/acl2ml/mcp/definitions/global/~A" safe-name))
             (definitions (extract-acl2-definitions-from-file book-file)))

        (when definitions
          (ensure-directories-exist (directory-namestring global-def-file))
          (with-open-file (stream global-def-file
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
            (format stream "~S~%" definitions))
          (format t "✓ Generated ~A definitions -> ~A~%"
                  (length definitions) global-def-file))
        (unless definitions
          (format t "✗ No definitions found in ~A~%" book-file)))))

  ;; List generated files
  (format t "~%Generated global definition files:~%")
  (let ((global-files (directory "/workspaces/acl2ml/mcp/definitions/global/*")))
    (dolist (file global-files)
      (format t "• ~A~%" (file-namestring file)))))