;;;; start-acl2ml-mcp.lisp
;;;; Start ACL2(ml) MCP server using saved_acl2

;; This file should be run with: /home/acl2/saved_acl2 < start-acl2ml-mcp.lisp

;; First we're in ACL2 mode - switch to Common Lisp
:q

;; Now we're in Common Lisp mode - load MCP server
(format t "~%=== ACL2(ml) MCP Server Starting ===~%")

;; Load dependencies
(ql:quickload :jsonrpc :silent t)
(ql:quickload "jsonrpc/transport/stdio" :silent t)
(ql:quickload :40ants-mcp :silent t)

(format t "✓ MCP dependencies loaded~%")

;; Define our package
(defpackage #:acl2ml-mcp
  (:use #:cl)
  (:export #:start-server #:analyze-expression #:analyze-theorem #:test-acl2))

(in-package :acl2ml-mcp)

;; Configuration
(defparameter *acl2-binary-path* "/home/acl2/saved_acl2")
(defparameter *acl2-books-dir* "/home/acl2/books/")

;; Core functionality
(defun extract-list-structure (expr &optional (level 1) (result nil))
  "Extract structural features from an S-expression"
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

(defstruct feature-vector symbols structure complexity)

(defun extract-features (expr)
  "Extract features from ACL2 expression"
  (let ((symbols (extract-list-structure expr)))
    (make-feature-vector
     :symbols symbols
     :structure (list :depth (reduce #'max symbols :key #'third :initial-value 0)
                      :nodes (length symbols))
     :complexity (length symbols))))

(defstruct theorem-analysis name logical-structure complexity-score)

(defun analyze-theorem-structure (theorem-form)
  "Analyze theorem structure"
  (when (and (listp theorem-form) (>= (length theorem-form) 2))
    (let* ((name (when (symbolp (first theorem-form)) (first theorem-form)))
           (formula (if name (second theorem-form) (first theorem-form))))
      (make-theorem-analysis
       :name name
       :logical-structure (cond
                           ((and (listp formula) (eq (first formula) 'implies)) 'implies)
                           ((and (listp formula) (eq (first formula) 'equal)) 'equal)
                           (t 'direct))
       :complexity-score (length (extract-list-structure formula))))))

(format t "✓ Core ACL2(ml) functionality loaded~%")

;; Define MCP tools
(40ants-mcp/tools:define-tool analyze-expression (expression)
  "Analyze an ACL2 expression and extract ML features"
  (handler-case
      (let* ((expr (read-from-string expression))
             (features (extract-features expr)))
        (list (make-instance '40ants-mcp/content/text:text-content
                             :text (format nil "ACL2(ml) Expression Analysis:~%~
                                           Input: ~A~%~
                                           Features extracted: ~A~%~
                                           Structure depth: ~A~%~
                                           Total nodes: ~A~%~
                                           Symbol breakdown: ~{~A~^, ~}~%"
                                          expr
                                          (length (feature-vector-symbols features))
                                          (getf (feature-vector-structure features) :depth)
                                          (getf (feature-vector-structure features) :nodes)
                                          (mapcar #'first (subseq (feature-vector-symbols features) 0
                                                                 (min 5 (length (feature-vector-symbols features)))))))))
    (error (e)
      (list (make-instance '40ants-mcp/content/text:text-content
                           :text (format nil "Error analyzing expression: ~A" e))))))

(40ants-mcp/tools:define-tool analyze-theorem (theorem)
  "Analyze the logical structure of an ACL2 theorem"
  (handler-case
      (let* ((expr (read-from-string theorem))
             (analysis (analyze-theorem-structure expr)))
        (list (make-instance '40ants-mcp/content/text:text-content
                             :text (format nil "ACL2(ml) Theorem Analysis:~%~
                                           Theorem: ~A~%~
                                           Name: ~A~%~
                                           Logical structure: ~A~%~
                                           Complexity score: ~A~%~
                                           ~%This analysis can be used for finding similar theorems~
                                           ~%and suggesting proof strategies."
                                          expr
                                          (or (theorem-analysis-name analysis) "anonymous")
                                          (theorem-analysis-logical-structure analysis)
                                          (theorem-analysis-complexity-score analysis)))))
    (error (e)
      (list (make-instance '40ants-mcp/content/text:text-content
                           :text (format nil "Error analyzing theorem: ~A" e))))))

(40ants-mcp/tools:define-tool test-acl2 ()
  "Test ACL2 integration and show system status"
  (list (make-instance '40ants-mcp/content/text:text-content
                       :text (format nil "ACL2(ml) System Status:~%~
                                     ACL2 Binary: ~A~%~
                                     Binary Status: ~A~%~
                                     ACL2 Books: ~A~%~
                                     Books Status: ~A~%~
                                     ~%Running in: Common Lisp mode (via :q from ACL2)~%~
                                     Can return to ACL2 with: (lp)~%~
                                     System: Ready for theorem proving assistance!"
                                    *acl2-binary-path*
                                    (if (probe-file *acl2-binary-path*) "✓ Found" "✗ Not Found")
                                    *acl2-books-dir*
                                    (if (probe-file *acl2-books-dir*) "✓ Found" "✗ Not Found")))))

;; Define the API
(openrpc-server:define-api acl2ml-tools
  (:title "ACL2(ml) Machine Learning Assistant for ACL2 Theorem Proving"
   :version "2.0.0"))

(format t "✓ MCP tools registered~%")
(format t "~%Starting ACL2(ml) MCP Server...~%")
(format t "Available tools:~%")
(format t "  • analyze-expression - Extract ML features from ACL2 expressions~%")
(format t "  • analyze-theorem - Analyze theorem logical structure~%")
(format t "  • test-acl2 - Check ACL2 system status~%")
(format t "~%Server ready for MCP connections!~%")

;; Start the MCP server
(40ants-mcp/server/definition:start-server 'acl2ml-tools)