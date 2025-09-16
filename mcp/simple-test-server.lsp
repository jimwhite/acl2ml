:q
; Simple test MCP server with basic demo
(load "~/quicklisp/setup.lisp")
(ql:quickload :40ants-mcp :silent t)

; Load modules
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")

(in-package :acl2ml-mcp)

; Simple test function that we know works
(defun simple-demo ()
  "Simple ACL2(ml) test that doesn't hang"
  (let ((test-expr '(defun test-fn (x) (+ x 1))))
    (handler-case
        (let* ((info (extract-info test-expr))
               (table (build-table info))
               (populated (populate-table table))
               (vector (flatten-table populated)))
          (format nil "ACL2(ml) Simple Test Results:~%~
                      Function: ~A~%~
                      Feature vector length: ~A~%~
                      Sample values: ~A~%~
                      ✅ Feature extraction working!"
                 (cadr test-expr)
                 (length (cadr vector))
                 (subseq (cadr vector) 0 5)))
      (error (e)
        (format nil "Error: ~A" e)))))

; Define API
(openrpc-server:define-api (acl2ml-simple :title "ACL2(ml) Simple Test"))

; Define simple tool
(40ants-mcp/tools:define-tool (acl2ml-simple test) ()
  (:summary "Simple ACL2(ml) feature extraction test")
  (:result (:type "array" :items (:type "object")))
  (list (make-instance '40ants-mcp/content/text:text-content
                       :text (simple-demo))))

; Start server
(40ants-mcp/server/definition:start-server acl2ml-simple :transport :stdio)