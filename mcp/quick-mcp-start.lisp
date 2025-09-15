;;;; quick-mcp-start.lisp
;;;; Quick start script for ACL2(ml) MCP server

(format t "Starting ACL2(ml) MCP Server...~%")

;; Load dependencies
(ql:quickload :jsonrpc :silent t)
(ql:quickload "jsonrpc/transport/stdio" :silent t)
(ql:quickload :40ants-mcp :silent t)

;; Load basic packages
(load "package-minimal.lisp")

(in-package :acl2ml-mcp)

;; Set up configuration
(defparameter *acl2-binary-path* "/home/acl2/saved_acl2")
(defparameter *acl2-books-dir* "/home/acl2/books/")

;; Load core functionality
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

;; Simple MCP server using 40ants-mcp
(format t "✓ Core functionality loaded~%")

;; Define tools using the correct 40ants-mcp API
(40ants-mcp/tools:define-tool analyze-expression (expression)
  "Analyze an ACL2 expression and extract features"
  (handler-case
      (let* ((expr (read-from-string expression))
             (features (extract-features expr)))
        (list (make-instance '40ants-mcp/content/text:text-content
                             :text (format nil "Expression Analysis:~%~
                                           Features: ~A~%~
                                           Depth: ~A, Nodes: ~A~%"
                                          (feature-vector-symbols features)
                                          (getf (feature-vector-structure features) :depth)
                                          (getf (feature-vector-structure features) :nodes)))))
    (error (e)
      (list (make-instance '40ants-mcp/content/text:text-content
                           :text (format nil "Error analyzing expression: ~A" e))))))

(40ants-mcp/tools:define-tool analyze-theorem (theorem)
  "Analyze the structure of an ACL2 theorem"
  (handler-case
      (let* ((expr (read-from-string theorem))
             (analysis (analyze-theorem-structure expr)))
        (list (make-instance '40ants-mcp/content/text:text-content
                             :text (format nil "Theorem Analysis:~%~
                                           Name: ~A~%~
                                           Structure: ~A~%~
                                           Complexity: ~A~%"
                                          (theorem-analysis-name analysis)
                                          (theorem-analysis-logical-structure analysis)
                                          (theorem-analysis-complexity-score analysis)))))
    (error (e)
      (list (make-instance '40ants-mcp/content/text:text-content
                           :text (format nil "Error analyzing theorem: ~A" e))))))

(40ants-mcp/tools:define-tool test-acl2 ()
  "Test ACL2 connection"
  (list (make-instance '40ants-mcp/content/text:text-content
                       :text (format nil "ACL2 Binary: ~A~%Status: ~A~%Books: ~A~%"
                                    *acl2-binary-path*
                                    (if (probe-file *acl2-binary-path*) "Found" "Not Found")
                                    (if (probe-file *acl2-books-dir*) "Found" "Not Found")))))

;; Define the API
(openrpc-server:define-api acl2ml-tools
  (:title "ACL2(ml) Machine Learning Assistant"
   :version "2.0.0"))

(format t "✓ MCP tools defined~%")
(format t "Starting MCP server on STDIO...~%")

;; Start the server
(40ants-mcp/server/definition:start-server 'acl2ml-tools)