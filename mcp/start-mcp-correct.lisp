;; First we're in ACL2 mode when saved_acl2 starts
;; Switch to Common Lisp mode where Quicklisp is available
:q

;; Now in Common Lisp - load dependencies
(ql:quickload :jsonrpc :silent t)
(ql:quickload "jsonrpc/transport/stdio" :silent t)
(ql:quickload :40ants-mcp :silent t)

(format t "~%=== ACL2(ml) MCP Server ===~%")
(format t "✓ Dependencies loaded in Common Lisp mode~%")

;; Define package
(defpackage #:acl2ml-mcp
  (:use #:cl)
  (:export #:start-server))

(in-package :acl2ml-mcp)

;; Core ACL2(ml) functionality
(defun extract-list-structure (expr &optional (level 1) (result nil))
  (when expr
    (if (atom expr)
        (append result (list (list expr 0 level)))
        (let ((head-result (list (list (car expr) (length (cdr expr)) level))))
          (reduce (lambda (acc item)
                    (if (listp item)
                        (append acc (extract-list-structure item (1+ level) nil))
                        (append acc (list (list item 0 (1+ level))))))
                  (cdr expr)
                  :initial-value (append result head-result))))))

;; Define MCP tools
(40ants-mcp/tools:define-tool analyze-expression (expression)
  "Analyze ACL2 expression for ML features"
  (handler-case
      (let* ((expr (read-from-string expression))
             (features (extract-list-structure expr)))
        (list (make-instance '40ants-mcp/content/text:text-content
                             :text (format nil "ACL2(ml) Analysis:~%Expression: ~A~%Features: ~A~%Depth: ~A~%"
                                          expr
                                          (length features)
                                          (reduce #'max features :key #'third :initial-value 0)))))
    (error (e)
      (list (make-instance '40ants-mcp/content/text:text-content
                           :text (format nil "Error: ~A" e))))))

(40ants-mcp/tools:define-tool test-acl2 ()
  "Test ACL2 system status"
  (list (make-instance '40ants-mcp/content/text:text-content
                       :text (format nil "ACL2(ml) Status:~%✓ Running in Common Lisp mode~%✓ Can return to ACL2 with (lp)~%✓ MCP server active"))))

;; Define API
(openrpc-server:define-api acl2ml-tools
  (:title "ACL2(ml) ML Assistant" :version "2.0.0"))

(format t "✓ MCP tools ready~%")
(format t "Starting server...~%")

;; Start MCP server
(40ants-mcp/server/definition:start-server 'acl2ml-tools)