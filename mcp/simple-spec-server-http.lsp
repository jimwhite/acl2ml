:q
; Simple HTTP MCP server that accepts ACL2 content as parameter (spec-compliant)
(load "~/quicklisp/setup.lisp")
(ql:quickload :40ants-mcp :silent t)

; Load modules
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")
(load "/workspaces/acl2ml/mcp/weka-clustering.lisp")

(in-package :acl2ml-mcp)

; Import the correct text-content class
(use-package :40ants-mcp/content/text)

; Simple function that processes ACL2 content from client
(defun process-acl2-content (acl2-content)
  "Process ACL2 content sent by client and return clustering results"
  (let ((expressions
         (let ((expressions '())
               (stream (make-string-input-stream acl2-content)))
           (handler-case
               (loop
                 (let ((expr (read stream nil :eof)))
                   (if (eq expr :eof)
                       (return (nreverse expressions))
                       (push expr expressions))))
             (error (e) (nreverse expressions)))))
        (feature-vectors '()))

    ; Process defun expressions
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

    ; Simple clustering
    (let ((clusters (if (> (length feature-vectors) 0)
                       (weka-cluster-definitions feature-vectors
                                               :algorithm "k-means"
                                               :granularity 3)
                       '())))
      (format nil "ACL2(ml) Clustering Results:~%~
                  Processed ~A definitions~%~
                  Generated ~A feature vectors~%~
                  Found ~A clusters~%~
                  ✅ Clustering complete!"
             (length feature-vectors)
             (length feature-vectors)
             (length clusters)))))

; Define API for HTTP transport
(openrpc-server:define-api (acl2ml-simple-http :title "ACL2(ml) Simple HTTP Clustering"))

; Define simple clustering tool that accepts content
(40ants-mcp/tools:define-tool (acl2ml-simple-http cluster) (content)
  (:summary "Cluster ACL2 definitions from provided content")
  (:param content string "ACL2 code to process and cluster")
  (:result (:type "array" :items (:type "object")))
  (list (make-instance 'text-content
                       :text (process-acl2-content content))))

(format t "🚀 Starting ACL2(ml) Simple HTTP MCP Server on port 8081...~%")

; Start HTTP server on port 8081 (different from the main HTTP server)
(40ants-mcp/server/definition:start-server acl2ml-simple-http
                                           :transport :http
                                           :port 8081)

(format t "✅ HTTP MCP Server running on port 8081!~%")

; Keep the server running
(loop (sleep 1))