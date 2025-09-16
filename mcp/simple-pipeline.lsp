:q
; Now in Common Lisp mode in saved_acl2
; Load our extraction functions
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")
(in-package :acl2ml-mcp)

; Simple pipeline without global state - just process one function at a time
(defun load-file-content (filepath)
  (with-open-file (stream filepath :direction :input)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))

(defun parse-acl2-expressions (acl2-string)
  (let ((expressions '())
        (stream (make-string-input-stream acl2-string)))
    (handler-case
        (loop
          (let ((expr (read stream nil :eof)))
            (if (eq expr :eof)
                (return (nreverse expressions))
                (push expr expressions))))
      (error (e)
        (format t "Warning: Parse error: ~A~%" e)
        (nreverse expressions)))))

(defun simple-pipeline-test ()
  (let ((example-content (load-file-content "/workspaces/acl2ml/manual/example.lisp")))
    (format t "Testing simple pipeline without global state~%")

    ; Parse expressions
    (let ((expressions (parse-acl2-expressions example-content))
          (definitions '()))

      (format t "Parsed ~A expressions~%" (length expressions))

      ; Process each defun individually
      (dolist (expr expressions)
        (when (and (listp expr) (eq (car expr) 'defun))
          (format t "Processing defun: ~A~%" (cadr expr))

          ; Just extract-info, no global state
          (handler-case
              (let ((info (extract-info expr)))
                (format t "  Extract-info: ~A~%" info)
                (push info definitions))
            (error (e)
              (format t "  Failed: ~A~%" e)))))

      (format t "Successfully processed ~A definitions~%" (length definitions))
      definitions)))

; Run the test
(simple-pipeline-test)

; Exit
(sb-ext:quit)