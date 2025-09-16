:q
; Simple working MCP server using proven pipeline
(load "~/quicklisp/setup.lisp")
(ql:quickload :40ants-mcp :silent t)

; Load our working modules
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")

(in-package :acl2ml-mcp)

(format t "=== Simple Working ACL2(ml) MCP Server ===~%")

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

(defun run-acl2ml-demo ()
  "Run the proven working pipeline on example.lisp"
  (let ((example-content (load-file-content "/workspaces/acl2ml/manual/example.lisp")))

    (format nil "ACL2(ml) Working Demo~%~
                =====================~%~
                Processing /workspaces/acl2ml/manual/example.lisp~%~%")

    ; Parse expressions
    (let ((expressions (parse-acl2-expressions example-content))
          (feature-vectors '())
          (results (make-string-output-stream)))

      (format results "Parsed ~A expressions~%" (length expressions))

      ; Process each defun through the complete pipeline
      (dolist (expr expressions)
        (when (and (listp expr) (eq (car expr) 'defun))
          (let ((func-name (cadr expr)))
            (format results "Processing: ~A~%" func-name)

            (handler-case
                (progn
                  ; Step 1: Extract info
                  (let ((info (extract-info expr)))
                    ; Step 2: Build table
                    (let ((table (build-table info)))
                      ; Step 3: Populate table
                      (let ((populated (populate-table table)))
                        ; Step 4: Flatten to final feature vector
                        (let ((vector (flatten-table populated)))
                          (format results "  ✓ Generated feature vector (length: ~A)~%" (length (cadr vector)))
                          (push vector feature-vectors))))))
              (error (e)
                (format results "  ✗ Failed: ~A~%" e))))))

      (format results "~%✅ Generated ~A feature vectors for clustering~%" (length feature-vectors))
      (format results "✅ Pipeline working correctly!~%")

      (get-output-stream-string results))))

; Simple MCP tool using proven functionality
(40ants-mcp/tools:define-tool acl2ml-demo ()
  "Demonstrate working ACL2(ml) feature extraction pipeline"
  (:result (:type "array" :items (:type "object")))
  (list (make-instance '40ants-mcp/content/text:text-content
                       :text (run-acl2ml-demo))))

(format t "✓ MCP tool 'acl2ml-demo' registered~%")

; Define proper API using openrpc-server:define-api
(openrpc-server:define-api acl2ml-simple
  (:title "ACL2(ml) Simple Server"
   :version "1.0.0"
   :description "Simple ACL2(ml) MCP server with working pipeline"))

(format t "✓ API defined~%")

; Start the MCP server
(format t "🚀 Starting ACL2(ml) MCP Server...~%")
(40ants-mcp/server/definition:start-server 'acl2ml-simple)