;;;; acl2-simple-extractor.lisp
;;;; Simple approach: Use ACL2's reader in Common Lisp mode to parse files

;; Load the extraction functions first
(load "extraction.lisp")
(load "table-to-feature-vector.lisp")

(defpackage #:acl2-simple-extractor
  (:use #:cl #:acl2ml-complete-original)
  (:export #:test-acl2-parsing))

(in-package :acl2-simple-extractor)

(defun test-single-file-with-acl2 (lisp-file)
  "Test parsing a single file using ACL2"
  (let ((script-content (format nil ":q
(defun read-acl2-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((forms nil))
      (handler-case
          (loop for form = (read stream nil :eof)
                until (eq form :eof)
                do (push form forms))
        (error (e)
          (format t \"Error reading ~A: ~A~%\" filename e)))
      (reverse forms))))

(let ((forms (read-acl2-file \"~A\")))
  (format t \"Read ~A forms from ~A~%\" (length forms) \"~A\"))

(quit)
" lisp-file lisp-file)))

    ;; Write script to temp file
    (with-open-file (stream "/tmp/test-script.lsp" :direction :output :if-exists :supersede)
      (write-string script-content stream))

    ;; Run with saved_acl2
    (format t "Testing ACL2 parsing of ~A...~%" lisp-file)
    (uiop:run-program "cd /home/acl2 && ./saved_acl2 < /tmp/test-script.lsp"
                      :output t :error-output t)))

(defun test-acl2-parsing ()
  "Test ACL2 parsing with a simple example"
  (let ((test-file "/home/acl2/books/arithmetic-3/bind-free/basic.lisp"))
    (if (probe-file test-file)
        (test-single-file-with-acl2 test-file)
      (format t "Test file ~A not found~%" test-file))))