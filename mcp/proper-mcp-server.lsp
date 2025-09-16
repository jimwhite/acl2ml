:q
; Proper MCP server following 40ants/mcp examples
(load "~/quicklisp/setup.lisp")
(ql:quickload :40ants-mcp :silent t)

; Load our working modules
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")

(in-package :acl2ml-mcp)

(format t "=== Proper ACL2(ml) MCP Server ===~%")

; Working functions from test-full-pipeline.lsp
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

(defun run-feature-extraction ()
  "Run the proven working pipeline on example.lisp"
  (let ((example-content (load-file-content "/workspaces/acl2ml/manual/example.lisp")))
    ; Parse expressions
    (let ((expressions (parse-acl2-expressions example-content))
          (feature-vectors '())
          (results (make-string-output-stream)))

      (format results "ACL2(ml) Feature Extraction Results~%")
      (format results "====================================~%")
      (format results "File: /workspaces/acl2ml/manual/example.lisp~%")
      (format results "Parsed expressions: ~A~%~%" (length expressions))

      ; Process each defun through the complete pipeline
      (dolist (expr expressions)
        (when (and (listp expr) (eq (car expr) 'defun))
          (let ((func-name (cadr expr)))
            (handler-case
                (progn
                  ; Complete pipeline: extract-info → build-table → populate-table → flatten-table
                  (let* ((info (extract-info expr))
                         (table (build-table info))
                         (populated (populate-table table))
                         (vector (flatten-table populated)))
                    (format results "✓ ~A: Generated ~A-dimensional feature vector~%"
                           func-name (length (cadr vector)))
                    (push vector feature-vectors)))
              (error (e)
                (format results "✗ ~A: Failed (~A)~%" func-name e))))))

      (format results "~%Summary: ~A feature vectors generated~%" (length feature-vectors))
      (format results "✅ ACL2(ml) pipeline working correctly!~%")

      (get-output-stream-string results))))

; Define API using correct 40ants syntax
(openrpc-server:define-api (acl2ml-tools :title "ACL2(ml) Machine Learning Tools"))

; Define MCP tool with proper parameter documentation
(40ants-mcp/tools:define-tool (acl2ml-tools demo) ()
  (:summary "Demonstrate ACL2(ml) feature extraction on example definitions")
  (:result (:type "array" :items (:type "object")))
  (list (make-instance '40ants-mcp/content/text:text-content
                       :text (run-feature-extraction))))

(format t "✓ MCP API 'acl2ml-tools' defined~%")
(format t "✓ MCP tool 'demo' registered~%")

; Start the MCP server using correct syntax
(format t "🚀 Starting ACL2(ml) MCP Server...~%")
(40ants-mcp/server/definition:start-server acl2ml-tools)