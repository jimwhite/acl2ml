:q
; Copy of simple-test-server but with content parameter - minimal change
(load "~/quicklisp/setup.lisp")
(ql:quickload :40ants-mcp :silent t)

; Load modules
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")

(in-package :acl2ml-mcp)

; Test function that we know works (from simple-test-server)
(defun test-with-content (content)
  "Test that worked before, now with content parameter"
  (let ((test-expr '(defun test-fn (x) (+ x 1))))
    (handler-case
        (let* ((info (extract-info test-expr))
               (table (build-table info))
               (populated (populate-table table))
               (vector (flatten-table populated)))
          (format nil "Content received: ~A chars. Test result: vector length ~A"
                 (length content)
                 (length (cadr vector))))
      (error (e)
        (format nil "Error: ~A" e)))))

; Define API (same as working version)
(openrpc-server:define-api (acl2ml-minimal :title "Minimal Working Test"))

; Define tool (same as working version but with parameter)
(40ants-mcp/tools:define-tool (acl2ml-minimal test) (content)
  (:summary "Test with content parameter")
  (:param content string "Content to process")
  (:result (:type "array" :items (:type "object")))
  (list (make-instance '40ants-mcp/content/text:text-content
                       :text (test-with-content content))))

; Start server (same as working version)
(40ants-mcp/server/definition:start-server acl2ml-minimal :transport :stdio)