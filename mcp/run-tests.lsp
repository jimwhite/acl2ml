:q
; Now in Common Lisp mode in saved_acl2
; Load our extraction functions
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")
(load "/workspaces/acl2ml/mcp/extraction-bridge.lisp")
(use-package :acl2ml-mcp)

; Load the test file
(load "/workspaces/acl2ml/mcp/test-core-pipeline.lisp")

; Now run the tests
; (run-all-tests)
(test-example-lisp)

; Exit
(quit)