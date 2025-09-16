:q
; Minimal MCP server test
(load "~/quicklisp/setup.lisp")
(ql:quickload :40ants-mcp :silent t)

; Load our modules
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")
(load "/workspaces/acl2ml/mcp/weka-clustering.lisp")

(in-package :acl2ml-mcp)

(format t "=== Minimal ACL2(ml) MCP Server ===~%")

; Simple test function
(defun simple-demo ()
  "Basic clustering demo"
  (format nil "ACL2(ml) MCP Server is running!~%~
              ✓ Feature extraction pipeline loaded~%~
              ✓ Clustering algorithms available~%~
              Ready for AI assistant integration."))

; Define minimal MCP tool
(40ants-mcp/tools:define-tool demo ()
  "Test ACL2(ml) functionality"
  (:result (:type "array" :items (:type "object")))
  (list (make-instance '40ants-mcp/content/text:text-content
                       :text (simple-demo))))

(format t "✓ MCP tool registered~%")

; Define API
(openrpc-server/api:define-api acl2ml-minimal
  (:title "ACL2(ml) Minimal Test Server"
   :version "1.0.0"
   :description "Minimal ACL2(ml) MCP server"))

; Start server
(format t "🚀 Starting ACL2(ml) MCP Server...~%")
(40ants-mcp/server/definition:start-server 'acl2ml-minimal)