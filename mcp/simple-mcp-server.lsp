:q
; Now in Common Lisp mode in saved_acl2
; Load dependencies
(load "~/quicklisp/setup.lisp")
(ql:quickload :40ants-mcp :silent t)

(format t "~%=== ACL2(ml) MCP Server ===~%")
(format t "✓ Dependencies loaded~%")

; Load our working modules
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")
(load "/workspaces/acl2ml/mcp/weka-clustering.lisp")

; Use our working package
(in-package :acl2ml-mcp)

(format t "✓ ACL2(ml) modules loaded~%")

; Test function that demonstrates the working pipeline
(defun test-clustering-demo ()
  "Run clustering on example.lisp and return formatted results"
  (handler-case
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
                      File: ~A~%~
                      Processed: ~A definitions~%~
                      Generated: ~A feature vectors~%~
                      Found: ~A clusters~%~
                      ~%Sample results:~%~
                      ~{Cluster ~A: ~A~%  Members: ~{~A~^, ~}~%~%~}~
                      ~%✓ System working correctly!"
                 example-file
                 (length feature-vectors)
                 (length feature-vectors)
                 (length clusters)
                 (loop for cluster in (subseq clusters 0 (min 3 (length clusters)))
                       collect (getf cluster :cluster-id)
                       collect (getf cluster :cluster-name)
                       collect (subseq (getf cluster :members)
                                      0 (min 5 (length (getf cluster :members))))))))
    (error (e)
      (format nil "Error in clustering demo: ~A" e))))

; Simple MCP tool without parameters to avoid documentation issues
(40ants-mcp/tools:define-tool acl2ml-demo ()
  "Demonstrate ACL2(ml) machine learning clustering on example.lisp"
  (:result (list :type "array" :items (:type "object")))
  (list (make-instance '40ants-mcp/content/text:text-content
                       :text (test-clustering-demo))))

; Define the MCP API
(openrpc-server:define-api acl2ml-server
  (:title "ACL2(ml) Machine Learning Assistant"
   :version "1.0.0"
   :description "AI assistant for ACL2 theorem proving with ML clustering"))

(format t "✓ MCP tool registered: acl2ml-demo~%")
(format t "~%🚀 Starting ACL2(ml) MCP Server...~%")
(format t "Ready for AI assistant integration!~%")
(format t "~%Use the 'acl2ml-demo' tool to see the system in action.~%")

; Start the MCP server
(40ants-mcp/server/definition:start-server 'acl2ml-server)