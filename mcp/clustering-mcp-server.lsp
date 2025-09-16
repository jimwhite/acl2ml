:q
; MCP server that clusters example.lisp - the minimal test
(load "~/quicklisp/setup.lisp")
(ql:quickload :40ants-mcp :silent t)

; Load our modules
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")
(load "/workspaces/acl2ml/mcp/weka-clustering.lisp")

(in-package :acl2ml-mcp)

; Core clustering functionality
(defun cluster-example-lisp ()
  "Cluster the definitions in example.lisp - the minimal test"
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

      ; Extract feature vectors
      (dolist (expr expressions)
        (when (and (listp expr) (eq (car expr) 'defun))
          (handler-case
              (let* ((info (extract-info expr))
                     (table (build-table info))
                     (populated (populate-table table))
                     (vector (flatten-table populated)))
                (push vector feature-vectors))
            (error (e) nil))))

      (setf feature-vectors (nreverse feature-vectors))

      ; Run clustering using our mock Weka clustering
      (let ((clusters (if (> (length feature-vectors) 0)
                         (weka-cluster-definitions feature-vectors
                                                 :algorithm "k-means"
                                                 :granularity 3)
                         '())))

        ; Format results
        (format nil "ACL2(ml) Clustering Results~%~
                    ============================~%~
                    File: /workspaces/acl2ml/manual/example.lisp~%~
                    Definitions processed: ~A~%~
                    Feature vectors generated: ~A~%~
                    Clusters found: ~A~%~
                    ~%~{Cluster ~A (~A members): ~{~A~^, ~}~%~}~
                    ~%✅ Clustering complete!"
               (length feature-vectors)
               (length feature-vectors)
               (length clusters)
               (loop for cluster in clusters
                     collect (getf cluster :cluster-id)
                     collect (length (getf cluster :members))
                     collect (getf cluster :members))))))

; Define API
(openrpc-server:define-api (acl2ml-clustering :title "ACL2(ml) Clustering"))

; Define clustering tool
(40ants-mcp/tools:define-tool (acl2ml-clustering cluster) ()
  (:summary "Cluster ACL2 definitions from example.lisp using machine learning")
  (:result (:type "array" :items (:type "object")))
  (list (make-instance '40ants-mcp/content/text:text-content
                       :text (cluster-example-lisp))))

; Start server
(40ants-mcp/server/definition:start-server acl2ml-clustering :transport :stdio)