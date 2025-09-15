;;;; package.lisp
;;;; Package definition for ACL2(ml) MCP Server

(defpackage #:acl2ml-mcp
  (:use #:cl #:alexandria)
  (:import-from #:40ants-mcp/tools #:define-tool)
  (:import-from #:40ants-mcp/content/text #:text-content)
  (:import-from #:40ants-mcp/server/definition #:start-server)
  (:import-from #:openrpc-server #:define-api)
  (:export
   ;; Main entry point
   #:start-acl2ml-server

   ;; ACL2 interface
   #:*acl2-binary-path*
   #:*acl2-books-dir*
   #:with-acl2-session
   #:eval-in-acl2

   ;; Definitions index
   #:*definitions-index*
   #:load-definitions-index
   #:regenerate-definitions-index
   #:find-definition
   #:list-definitions

   ;; Feature extraction
   #:extract-features
   #:extract-list-structure
   #:compute-similarity

   ;; Lemma analysis
   #:analyze-theorem-structure
   #:suggest-similar-lemmas
   #:extract-dependencies))