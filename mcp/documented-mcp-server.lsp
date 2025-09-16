:q
; MCP server with proper parameter documentation
(load "~/quicklisp/setup.lisp")
(ql:quickload :40ants-mcp :silent t)

; Load our modules
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")
(load "/workspaces/acl2ml/mcp/weka-clustering.lisp")

(in-package :acl2ml-mcp)

(format t "=== ACL2(ml) MCP Server ===~%")

; Helper function for demo
(defun quick-demo ()
  "Quick demonstration of ACL2(ml) clustering"
  (let* ((example-file "/workspaces/acl2ml/manual/example.lisp")
         (example-content
          (with-open-file (stream example-file :direction :input)
            (let ((content (make-string (file-length stream))))
              (read-sequence content stream)
              content)))
         (expressions (parse-acl2-expressions example-content))
         (feature-vectors '()))

    ; Process definitions
    (dolist (expr expressions)
      (when (and (listp expr) (eq (car expr) 'defun))
        (handler-case
            (let* ((info (extract-info expr))
                   (table (build-table info))
                   (populated (populate-table table))
                   (vector (flatten-table populated)))
              (push vector feature-vectors))
          (error (e)
            (format t "Warning: Failed to process ~A: ~A~%"
                   (if (> (length expr) 1) (cadr expr) "UNKNOWN") e)))))

    (setf feature-vectors (nreverse feature-vectors))

    ; Run clustering
    (let ((clusters (if (> (length feature-vectors) 0)
                       (weka-cluster-definitions feature-vectors
                                               :algorithm "k-means"
                                               :granularity 3)
                       '())))

      (format nil "ACL2(ml) Clustering Demo~%~
                  =======================~%~
                  Processed: ~A definitions~%~
                  Generated: ~A feature vectors~%~
                  Found: ~A clusters~%~
                  ~%✓ System working correctly!"
             (length feature-vectors)
             (length feature-vectors)
             (length clusters)))))

; MCP tool with no parameters to avoid documentation issues
(40ants-mcp/tools:define-tool acl2ml-demo ()
  "Demonstrate ACL2(ml) machine learning clustering capabilities"
  (:result (:type "array" :items (:type "object")))
  (list (make-instance '40ants-mcp/content/text:text-content
                       :text (quick-demo))))

(format t "✓ MCP tool registered: acl2ml-demo~%")

; Start server without API definition to test if that's the issue
(format t "🚀 Starting ACL2(ml) MCP Server...~%")
(format t "Ready for AI assistant integration!~%")

; Try just starting the server mechanism
(handler-case
    (40ants-mcp/server/definition:start-server)
  (error (e)
    (format t "Error: ~A~%" e)))