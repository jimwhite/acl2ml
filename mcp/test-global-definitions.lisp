;;;; Test script to generate global definitions from ACL2 books
:q
(load "~/quicklisp/setup.lisp")
(load "/workspaces/acl2ml/mcp/library-export.lisp")

;; Generate definitions from some common ACL2 books
(in-package :acl2ml-library-export)

;; Test with a few specific ACL2 book files
(let ((test-books '("/home/acl2/acl2-8.6/books/arithmetic-5/lib/basic-ops/arithmetic-theory.lisp"
                   "/home/acl2/acl2-8.6/books/std/lists/len.lisp"
                   "/home/acl2/acl2-8.6/books/std/basic/defs.lisp")))

  (format t "~%TESTING GLOBAL DEFINITIONS GENERATION~%")
  (format t "====================================~%")

  (dolist (book-file test-books)
    (format t "~%Processing: ~A~%" book-file)
    (when (probe-file book-file)
      (let* ((safe-name (substitute-if #\_ (lambda (c) (member c '(#\/ #\.)))
                                      (enough-namestring book-file "/home/acl2/acl2-8.6/books/")))
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
                  (length definitions) global-def-file))))))