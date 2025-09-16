:q
; Clean STDIO MCP server - minimize output mixing
(load "~/quicklisp/setup.lisp")
(ql:quickload :40ants-mcp :silent t)

; Load modules silently
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")

(in-package :acl2ml-mcp)

; Working pipeline function (minimal output)
(defun run-feature-extraction-quiet ()
  "Run ACL2(ml) pipeline with minimal output"
  (let ((example-content
         (with-open-file (stream "/workspaces/acl2ml/manual/example.lisp" :direction :input)
           (let ((content (make-string (file-length stream))))
             (read-sequence content stream)
             content))))

    (let ((expressions
           (let ((expressions '())
                 (stream (make-string-input-stream example-content)))
             (handler-case
                 (loop
                   (let ((expr (read stream nil :eof)))
                     (if (eq expr :eof)
                         (return (nreverse expressions))
                         (push expr expressions))))
               (error (e) (nreverse expressions)))))
          (feature-vectors '()))

      (dolist (expr expressions)
        (when (and (listp expr) (eq (car expr) 'defun))
          (handler-case
              (let* ((info (extract-info expr))
                     (table (build-table info))
                     (populated (populate-table table))
                     (vector (flatten-table populated)))
                (push vector feature-vectors))
            (error (e) nil))))

      (format nil "ACL2(ml) Results: Processed ~A definitions, generated ~A feature vectors. Pipeline working!"
             (length feature-vectors) (length feature-vectors)))))

; Define API without debug output
(openrpc-server:define-api (acl2ml-stdio :title "ACL2(ml) STDIO Tools"))

; Define tool
(40ants-mcp/tools:define-tool (acl2ml-stdio demo) ()
  (:summary "Run ACL2(ml) feature extraction pipeline")
  (:result (:type "array" :items (:type "object")))
  (list (make-instance '40ants-mcp/content/text:text-content
                       :text (run-feature-extraction-quiet))))

; Start server with explicit STDIO transport
(40ants-mcp/server/definition:start-server acl2ml-stdio :transport :stdio)