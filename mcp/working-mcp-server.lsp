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

; Helper functions for MCP
(defun parse-acl2-expressions (acl2-string)
  "Parse ACL2 content string into list of S-expressions"
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

(defun process-definitions-to-vectors (acl2-content)
  "Process ACL2 definitions into feature vectors"
  (let ((expressions (parse-acl2-expressions acl2-content))
        (feature-vectors '()))

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

    (nreverse feature-vectors)))

; Define MCP tools using our working functions
(40ants-mcp/tools:define-tool acl2-cluster-analysis (acl2-content algorithm granularity-level)
  "Cluster ACL2 definitions using machine learning algorithms"
  (handler-case
      (let* ((granularity (or (ignore-errors (parse-integer granularity-level)) 3))
             (feature-vectors (process-definitions-to-vectors acl2-content))
             (clusters (if (> (length feature-vectors) 0)
                          (weka-cluster-definitions feature-vectors
                                                  :algorithm algorithm
                                                  :granularity granularity)
                          '())))

        (list (make-instance '40ants-mcp/content/text:text-content
                             :text (format nil "ACL2(ml) Clustering Analysis~%~
                                           ================================~%~
                                           Algorithm: ~A~%~
                                           Granularity: ~A~%~
                                           Total definitions: ~A~%~
                                           Clusters found: ~A~%~
                                           ~%~{Cluster ~A: ~A~%~
                                           Members: ~{~A~^, ~}~%~%~}"
                                          algorithm granularity
                                          (length feature-vectors)
                                          (length clusters)
                                          (loop for cluster in clusters
                                                collect (getf cluster :cluster-id)
                                                collect (getf cluster :cluster-name)
                                                collect (getf cluster :members))))))
    (error (e)
      (list (make-instance '40ants-mcp/content/text:text-content
                           :text (format nil "Error clustering definitions: ~A" e))))))

(40ants-mcp/tools:define-tool acl2-extract-features (acl2-expression)
  "Extract machine learning features from a single ACL2 expression"
  (handler-case
      (let* ((expr (read-from-string acl2-expression)))
        (if (and (listp expr) (eq (car expr) 'defun))
            (let* ((info (extract-info expr))
                   (table (build-table info))
                   (populated (populate-table table))
                   (vector (flatten-table populated)))

              (list (make-instance '40ants-mcp/content/text:text-content
                                   :text (format nil "ACL2(ml) Feature Extraction~%~
                                                 ==============================~%~
                                                 Function: ~A~%~
                                                 Raw info: ~A~%~
                                                 Feature vector: ~A~%~
                                                 Vector length: ~A dimensions~%~
                                                 ~%This uses the original ACL2(ml) arity-based~%~
                                                 feature encoding for clustering analysis."
                                                (cadr expr)
                                                info
                                                (cadr vector)
                                                (length (cadr vector))))))
            (list (make-instance '40ants-mcp/content/text:text-content
                                 :text "Error: Expression must be a defun form"))))
    (error (e)
      (list (make-instance '40ants-mcp/content/text:text-content
                           :text (format nil "Error extracting features: ~A" e))))))

(40ants-mcp/tools:define-tool acl2ml-demo ()
  "Demonstrate ACL2(ml) capabilities with example.lisp"
  (handler-case
      (let* ((example-file "/workspaces/acl2ml/manual/example.lisp")
             (example-content
              (with-open-file (stream example-file :direction :input)
                (let ((content (make-string (file-length stream))))
                  (read-sequence content stream)
                  content)))
             (feature-vectors (process-definitions-to-vectors example-content))
             (clusters (if (> (length feature-vectors) 0)
                          (weka-cluster-definitions feature-vectors
                                                  :algorithm "k-means"
                                                  :granularity 3)
                          '())))

        (list (make-instance '40ants-mcp/content/text:text-content
                             :text (format nil "ACL2(ml) System Demo~%~
                                           ===================~%~
                                           Using: ~A~%~
                                           ~%✓ Processed ~A definitions~%~
                                           ✓ Generated ~A feature vectors~%~
                                           ✓ Found ~A clusters~%~
                                           ~%Sample clusters:~%~
                                           ~{~A: ~{~A~^, ~}~%~}~
                                           ~%The system successfully extracts structural~%~
                                           patterns from ACL2 definitions for clustering!"
                                          example-file
                                          (length feature-vectors)
                                          (length feature-vectors)
                                          (length clusters)
                                          (loop for cluster in (subseq clusters 0 (min 3 (length clusters)))
                                                collect (getf cluster :cluster-name)
                                                collect (subseq (getf cluster :members)
                                                               0 (min 3 (length (getf cluster :members)))))))))
    (error (e)
      (list (make-instance '40ants-mcp/content/text:text-content
                           :text (format nil "Error running demo: ~A" e))))))

; Define the MCP API
(openrpc-server:define-api acl2ml-mcp-server
  (:title "ACL2(ml) Machine Learning Assistant"
   :version "1.0.0"
   :description "AI assistant for ACL2 theorem proving with ML clustering"))

(format t "✓ MCP tools registered:~%")
(format t "  • acl2-cluster-analysis~%")
(format t "  • acl2-extract-features~%")
(format t "  • acl2ml-demo~%")

(format t "~%🚀 Starting ACL2(ml) MCP Server...~%")
(format t "Ready for AI assistant integration!~%")

; Start the MCP server
(40ants-mcp/server/definition:start-server 'acl2ml-mcp-server)