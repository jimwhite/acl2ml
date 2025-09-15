;; Fixed ACL2(ml) MCP Server - Silent startup for JSON-RPC
:q

;; Load dependencies silently
(load "~/quicklisp/setup.lisp")
(ql:quickload :jsonrpc :silent t)
(ql:quickload "jsonrpc/transport/stdio" :silent t)
(ql:quickload :40ants-mcp :silent t)

;; Load modules silently
(load "/workspaces/acl2ml/mcp/clustering.lisp")
(load "/workspaces/acl2ml/mcp/feature-extraction-full.lisp")

;; Main package
(defpackage #:acl2ml-mcp-fixed
  (:use #:cl #:acl2ml-clustering #:acl2ml-features))

(in-package :acl2ml-mcp-fixed)

;; Load definitions index silently
(defparameter *definitions-index* nil)

(handler-case
    (with-open-file (stream "/workspaces/acl2ml/mcp/definitions-index.dat" :direction :input)
      (loop for line = (read-line stream nil)
            until (null line)
            unless (char= (char line 0) #\;)
            do (let ((def (ignore-errors (read-from-string line))))
                 (when (and def (= (length def) 4))
                   (push def *definitions-index*)))))
  (error (e) nil))

(setf *definitions-index* (nreverse *definitions-index*))

;; MCP Tools
(40ants-mcp/tools:define-tool cluster-example ()
  "Cluster the example.lisp definitions"
  (handler-case
      (let ((result (cluster-definitions *definitions-index* :k-means :granularity-level 3)))
        (list (make-instance '40ants-mcp/content/text:text-content
                             :text (with-output-to-string (s)
                                     (format-clustering-results result *definitions-index* :stream s)))))
    (error (e)
      (list (make-instance '40ants-mcp/content/text:text-content
                           :text (format nil "Clustering error: ~A" e))))))

(40ants-mcp/tools:define-tool find-similar (target-name)
  "Find similar definitions to target"
  (handler-case
      (let* ((target-symbol (read-from-string target-name))
             (similar (find-similar-items target-symbol *definitions-index* :k-means)))
        (list (make-instance '40ants-mcp/content/text:text-content
                             :text (with-output-to-string (s)
                                     (format-similarity-results (list target-symbol) similar :stream s)))))
    (error (e)
      (list (make-instance '40ants-mcp/content/text:text-content
                           :text (format nil "Similarity error: ~A" e))))))

;; API definition
(openrpc-server:define-api acl2ml-fixed
  (:title "ACL2(ml) Fixed MCP Server" :version "1.0.0"))

;; Start MCP server (this should handle JSON-RPC over STDIO)
(40ants-mcp/server/definition:start-server 'acl2ml-fixed)