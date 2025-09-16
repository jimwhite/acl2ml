:q
; Now in Common Lisp mode in saved_acl2
; Load our extraction functions
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")
(load "/workspaces/acl2ml/mcp/extraction-bridge.lisp")
(in-package :acl2ml-mcp)

; Test the complete pipeline
(defun load-file-content (filepath)
  (with-open-file (stream filepath :direction :input)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))

(defun test-complete-pipeline ()
  (let ((example-content (load-file-content "/workspaces/acl2ml/manual/example.lisp")))
    (format t "Testing complete extraction pipeline with example.lisp~%")

    ; Clear any previous state
    (clear-processing-state)

    ; Test the complete pipeline
    (let ((result (complete-extraction-pipeline example-content "definitions")))
      (format t "Pipeline result: ~A~%" result)

      (let ((vectors (getf result :vectors)))
        (format t "Generated ~A feature vectors~%" (length vectors))
        (when vectors
          (format t "First vector: ~A~%" (car vectors)))))))

; Run the test
(test-complete-pipeline)

; Exit
(sb-ext:quit)