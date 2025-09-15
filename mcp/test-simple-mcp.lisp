;;;; test-simple-mcp.lisp
;;;; Test the simple MCP server implementation

(format t "Testing Simple MCP Server Implementation...~%")

;; Load the required files
(load "package-minimal.lisp")
(format t "✓ Package loaded~%")

(in-package :acl2ml-mcp)

;; Set up basic configuration
(defparameter *acl2-binary-path* "/home/acl2/saved_acl2")
(defparameter *acl2-books-dir* "/home/acl2/books/")

;; Load the required functionality
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

(defstruct theorem-info
  name type hypothesis conclusion complexity)

(defun analyze-theorem-structure (theorem-form)
  "Analyze theorem structure"
  (when (and (listp theorem-form) (>= (length theorem-form) 2))
    (let* ((name (when (symbolp (first theorem-form)) (first theorem-form)))
           (formula (if name (second theorem-form) (first theorem-form))))
      (multiple-value-bind (hyp concl type)
          (cond
            ((and (listp formula) (eq (first formula) 'implies) (= (length formula) 3))
             (values (second formula) (third formula) 'implies))
            ((and (listp formula) (eq (first formula) 'equal) (= (length formula) 3))
             (values nil formula 'equal))
            (t (values nil formula 'direct)))
        (make-theorem-info
         :name name :type type :hypothesis hyp :conclusion concl
         :complexity (length (extract-list-structure formula)))))))

;; Load the MCP server
(load "simple-mcp-server.lisp")
(format t "✓ Simple MCP server loaded~%")

;; Test the functionality
(test-simple-mcp)

(format t "~%Testing individual tools...~%")

;; Test analyze-expression tool
(let ((result (funcall (mcp-tool-handler (gethash "analyze-expression" *mcp-tools*)))))
  (format t "✓ analyze-expression result: ~A~%" result))

;; Test theorem analysis tool
(let ((result (funcall (mcp-tool-handler (gethash "analyze-theorem" *mcp-tools*)))))
  (format t "✓ analyze-theorem result: ~A~%" result))

;; Test ACL2 connection
(let ((result (funcall (mcp-tool-handler (gethash "test-acl2-connection" *mcp-tools*)))))
  (format t "✓ test-acl2-connection result: ~A~%" result))

(format t "~%Simple MCP server tests completed!~%")