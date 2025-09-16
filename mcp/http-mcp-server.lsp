:q
; HTTP MCP server following 40ants/mcp documentation
(load "~/quicklisp/setup.lisp")
(ql:quickload :40ants-mcp :silent t)

; Load our working modules
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")

(in-package :acl2ml-mcp)

; Import the correct text-content class
(use-package :40ants-mcp/content/text)

(format t "=== HTTP ACL2(ml) MCP Server ===~%")

; Working pipeline function
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
  "Run the working ACL2(ml) pipeline"
  (let ((example-content (load-file-content "/workspaces/acl2ml/manual/example.lisp")))
    (let ((expressions (parse-acl2-expressions example-content))
          (feature-vectors '())
          (results (make-string-output-stream)))

      (format results "ACL2(ml) Feature Extraction Results~%")
      (format results "====================================~%")
      (format results "Processing: /workspaces/acl2ml/manual/example.lisp~%~%")

      (dolist (expr expressions)
        (when (and (listp expr) (eq (car expr) 'defun))
          (let ((func-name (cadr expr)))
            (handler-case
                (let* ((info (extract-info expr))
                       (table (build-table info))
                       (populated (populate-table table))
                       (vector (flatten-table populated)))
                  (format results "✓ ~A: ~A-dimensional vector~%"
                         func-name (length (cadr vector)))
                  (push vector feature-vectors))
              (error (e)
                (format results "✗ ~A: ~A~%" func-name e))))))

      (format results "~%Summary: ~A feature vectors generated~%" (length feature-vectors))
      (get-output-stream-string results))))

; Define API with HTTP transport
(openrpc-server:define-api (acl2ml-http-tools :title "ACL2(ml) HTTP Tools"))

; Define tool with proper documentation
(40ants-mcp/tools:define-tool (acl2ml-http-tools demo) ()
  (:summary "Demonstrate ACL2(ml) feature extraction pipeline")
  (:result (:type "array" :items (:type "object")))
  (list (make-instance 'text-content
                       :text (run-feature-extraction))))

(format t "✓ HTTP MCP API defined~%")
(format t "✓ Demo tool registered~%")

; Start HTTP server on port 8080
(format t "🚀 Starting HTTP MCP Server on port 8080...~%")
(40ants-mcp/server/definition:start-server acl2ml-http-tools
                                           :transport :http
                                           :port 8080)

(format t "✅ HTTP MCP Server running!~%")
(format t "Test with: curl -X POST http://localhost:8080 -H 'Content-Type: application/json' -d '{\"jsonrpc\":\"2.0\",\"method\":\"tools/list\",\"id\":1}'~%")

; Keep the server running
(loop (sleep 1))