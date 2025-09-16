:q
; MCP server implementing acl2_cluster_analysis per COMPLETE_IMPLEMENTATION_SPEC.md
(load "~/quicklisp/setup.lisp")
(ql:quickload :40ants-mcp :silent t)

; Define package with proper imports
(defpackage #:acl2ml-mcp
  (:use #:cl)
  (:import-from #:40ants-mcp/content/text
                #:text-content)
  (:export #:acl2-cluster-analysis))

(in-package :acl2ml-mcp)

; Load our modules
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")
(load "/workspaces/acl2ml/mcp/weka-clustering.lisp")

; Implementation of acl2_cluster_analysis per spec
(defun acl2-cluster-analysis (acl2-content content-type algorithm granularity)
  "Cluster similar ACL2 definitions or theorems using machine learning - per spec"

  ; Parse ACL2 content
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

    ; Extract feature vectors for specified content type
    (dolist (expr expressions)
      (when (and (listp expr)
                 (if (string= content-type "definitions")
                     (eq (car expr) 'defun)
                     (eq (car expr) 'defthm)))
        (handler-case
            (let* ((info (extract-info expr))
                   (table (build-table info))
                   (populated (populate-table table))
                   (vector (flatten-table populated)))
              (push vector feature-vectors))
          (error (e) nil))))

    (setf feature-vectors (nreverse feature-vectors))

    ; Run clustering
    (let* ((start-time (get-internal-real-time))
           (clusters (if (> (length feature-vectors) 0)
                        (weka-cluster-definitions feature-vectors
                                                :algorithm algorithm
                                                :granularity granularity)
                        '()))
           (end-time (get-internal-real-time))
           (processing-time (round (* 1000 (/ (- end-time start-time)
                                             internal-time-units-per-second)))))

      ; Return structured response per spec
      (list :clusters clusters
            :algorithm-used algorithm
            :granularity-level granularity
            :total-items (length feature-vectors)
            :processing-time-ms processing-time))))

; Define API
(openrpc-server:define-api (acl2ml-spec :title "ACL2(ml) MCP Server"))

; Define MCP tool per specification with proper error handling
(40ants-mcp/tools:define-tool (acl2ml-spec acl2-cluster-analysis) (acl2-content content-type algorithm granularity)
  (:summary "Cluster similar ACL2 definitions or theorems using machine learning")
  (:param acl2-content string "ACL2 code containing definitions/theorems to cluster")
  (:param content-type string "Whether to cluster function definitions or theorems")
  (:param algorithm string "Clustering algorithm (k-means, em, farthest-first)")
  (:param granularity integer "Granularity level: 1=big general groups, 5=small precise groups")
  (:result (:type "array" :items (:type "object")))
  (handler-case
      (let ((result (acl2-cluster-analysis acl2-content content-type algorithm granularity)))
        (list (make-instance 'text-content
                             :text (format nil "~A" result))))
    (error (e)
      (list (make-instance 'text-content
                           :text (format nil "Error processing clustering: ~A" e))))))

; Start HTTP server on port 8082
(format t "🚀 Starting Spec-Compliant ACL2(ml) HTTP MCP Server on port 8082...~%")
(40ants-mcp/server/definition:start-server acl2ml-spec
                                           :transport :http
                                           :port 8082)
(format t "✅ Spec-Compliant HTTP MCP Server running on port 8082!~%")

; Keep the server running
(loop (sleep 1))