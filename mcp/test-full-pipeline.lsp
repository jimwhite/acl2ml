:q
; Now in Common Lisp mode in saved_acl2
; Load our extraction functions
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")
(in-package :acl2ml-mcp)

; Test the full pipeline: extract-info → build-table → populate-table → flatten-table
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

(defun full-pipeline-test ()
  (let ((example-content (load-file-content "/workspaces/acl2ml/manual/example.lisp")))
    (format t "Testing FULL pipeline: extract → build → populate → flatten~%")

    ; Parse expressions
    (let ((expressions (parse-acl2-expressions example-content))
          (feature-vectors '()))

      (format t "Parsed ~A expressions~%" (length expressions))

      ; Process each defun through the complete pipeline
      (dolist (expr expressions)
        (when (and (listp expr) (eq (car expr) 'defun))
          (let ((func-name (cadr expr)))
            (format t "Processing: ~A~%" func-name)

            (handler-case
                (progn
                  ; Step 1: Extract info
                  (let ((info (extract-info expr)))
                    (format t "  1. Extract-info: ~A~%" info)

                    ; Step 2: Build table
                    (let ((table (build-table info)))
                      (format t "  2. Build-table: ~A~%" table)

                      ; Step 3: Populate table (convert symbols to numbers)
                      (let ((populated (populate-table table)))
                        (format t "  3. Populate-table: ~A~%" populated)

                        ; Step 4: Flatten to final feature vector
                        (let ((vector (flatten-table populated)))
                          (format t "  4. Feature vector: ~A~%" vector)
                          (push vector feature-vectors))))))
              (error (e)
                (format t "  Failed: ~A~%" e)))

            (format t "~%"))))

      (format t "Generated ~A feature vectors for clustering~%" (length feature-vectors))
      feature-vectors)))

; Run the test
(full-pipeline-test)

; Exit
(sb-ext:quit)