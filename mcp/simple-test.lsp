:q
; Now in Common Lisp mode in saved_acl2
; Load our extraction functions
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")
(load "/workspaces/acl2ml/mcp/extraction-bridge.lisp")
(in-package :acl2ml-mcp)

; Simple test - just load example.lisp content and try to process one function
(defun load-file-content (filepath)
  (with-open-file (stream filepath :direction :input)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))

(defun simple-test ()
  (let ((example-content (load-file-content "/workspaces/acl2ml/manual/example.lisp")))
    (format t "Loaded ~A characters~%" (length example-content))
    (format t "First 100 chars: ~A~%" (subseq example-content 0 100))

    ; Try just parsing the content first
    (let ((expressions (parse-acl2-expressions example-content)))
      (format t "Parsed ~A expressions~%" (length expressions))

      ; Try to process just the first defun
      (dolist (expr expressions)
        (when (and (listp expr) (eq (car expr) 'defun))
          (format t "Found defun: ~A~%" (cadr expr))
          (handler-case
              (let ((info (extract-info expr)))
                (format t "Extract-info succeeded: ~A~%" info))
            (error (e)
              (format t "Extract-info failed: ~A~%" e)))
          (return))))))

; Run the simple test
(simple-test)

; Exit
(sb-ext:quit)