#!/usr/bin/env sbcl --script
;;;; start-server.lisp
;;;; Startup script for ACL2(ml) MCP Server

;; Load ASDF
#-asdf (require :asdf)

;; Load the system
(asdf:load-system :acl2ml-mcp)

;; Import the main package
(use-package :acl2ml-mcp)

;; Parse command line arguments
(defun parse-args (args)
  "Parse command line arguments"
  (let ((transport :stdio)
        (port 8080)
        (acl2-binary nil)
        (books-dir nil))

    (loop for arg in args do
      (cond
        ((string= arg "--http") (setf transport :http))
        ((string= arg "--stdio") (setf transport :stdio))
        ((string-prefix-p "--port=" arg)
         (setf port (parse-integer (subseq arg 7))))
        ((string-prefix-p "--acl2-binary=" arg)
         (setf acl2-binary (subseq arg 14)))
        ((string-prefix-p "--books-dir=" arg)
         (setf books-dir (subseq arg 12)))))

    (values transport port acl2-binary books-dir)))

(defun string-prefix-p (prefix string)
  "Check if string starts with prefix"
  (and (>= (length string) (length prefix))
       (string= (subseq string 0 (length prefix)) prefix)))

;; Main entry point
(defun main ()
  "Main entry point for the server"
  (multiple-value-bind (transport port acl2-binary books-dir)
      (parse-args sb-ext:*posix-argv*)

    ;; Update configuration if provided
    (when acl2-binary
      (setf *acl2-binary-path* acl2-binary))
    (when books-dir
      (setf *acl2-books-dir* books-dir))

    ;; Start the server
    (start-acl2ml-server :transport transport :port port)))

;; Run if called as script
(when (member "--script" sb-ext:*posix-argv* :test #'string=)
  (main))