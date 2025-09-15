;;;; package-minimal.lisp
;;;; Minimal package definition for testing without dependencies

(defpackage #:acl2ml-mcp
  (:use #:cl)
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