;; Simple ACL2 definitions indexer
;; Run with: /home/acl2/saved_acl2 < simple-index.lisp

:q

(load "~/quicklisp/setup.lisp")

(format t "~%=== Simple ACL2 Index Generator ===~%")

(defpackage #:simple-indexer
  (:use #:cl))

(in-package :simple-indexer)

(defparameter *definitions* nil)
(defparameter *files-scanned* 0)

(defun scan-file-for-definitions (filepath)
  "Scan a single file for ACL2 definitions"
  (handler-case
      (with-open-file (stream filepath :direction :input)
        (loop for form = (read stream nil :eof)
              until (eq form :eof)
              when (and (listp form)
                       (>= (length form) 2)
                       (member (first form) '(defun defthm defmacro defconst)))
              do (push (list (second form)     ; name
                            (first form)      ; type
                            filepath          ; file
                            (length (format nil "~S" form))) ; complexity
                       *definitions*)))
    (error (e)
      (format t "Error reading ~A: ~A~%" filepath e))))

;; Scan key ACL2 book files
(defparameter *key-files*
  '("/home/acl2/books/arithmetic/top.lisp"
    "/home/acl2/books/std/lists/top.lisp"
    "/home/acl2/books/std/util/top.lisp"
    "/home/acl2/books/centaur/fty/top.lisp"
    "/home/acl2/books/std/basic/top.lisp"))

(format t "Scanning key ACL2 book files...~%")

(dolist (file *key-files*)
  (when (probe-file file)
    (format t "Scanning: ~A~%" file)
    (scan-file-for-definitions file)
    (incf *files-scanned*)))

;; Also scan the example file
(let ((example-file "/workspaces/acl2ml/manual/example.lisp"))
  (when (probe-file example-file)
    (format t "Scanning: ~A~%" example-file)
    (scan-file-for-definitions example-file)
    (incf *files-scanned*)))

(format t "~%Results:~%")
(format t "Files scanned: ~A~%" *files-scanned*)
(format t "Definitions found: ~A~%" (length *definitions*))

(with-open-file (stream "/workspaces/acl2ml/mcp/definitions-index.dat"
                        :direction :output
                        :if-exists :supersede)
  (format stream ";; ACL2 Definitions Index~%")
  (format stream ";; Files: ~A, Definitions: ~A~%~%"
          *files-scanned* (length *definitions*))

  (dolist (def (reverse *definitions*))
    (format stream "~S~%" def)))

(format t "Index saved to definitions-index.dat~%")

(sb-ext:exit)