;;;; acl2ml-mcp.asd
;;;; ASDF system definition for ACL2(ml) MCP Server

(defsystem "acl2ml-mcp"
  :description "ACL2(ml) Machine Learning Assistant as MCP Server"
  :author "Updated for ACL2 8.6 and MCP"
  :license "MIT"
  :version "2.0.0"
  :serial t
  :depends-on ("40ants-mcp"
               "alexandria"
               "cl-ppcre"
               "parse-number")
  :components ((:file "package")
               (:file "acl2-interface")
               (:file "definitions-index")
               (:file "feature-extraction")
               (:file "lemma-analysis")
               (:file "similarity")
               (:file "mcp-tools")
               (:file "server")))