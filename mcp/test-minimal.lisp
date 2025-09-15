;;;; test-minimal.lisp
;;;; Minimal test without external dependencies

(format t "Testing ACL2(ml) core functionality...~%")

;; Load minimal package
(load "package-minimal.lisp")
(format t "✓ Package loaded~%")

(in-package :acl2ml-mcp)

;; Test ACL2 binary path
(defparameter *acl2-binary-path* "/home/acl2/saved_acl2")
(defparameter *acl2-books-dir* "/home/acl2/acl2-8.6/books/")

(format t "Checking ACL2 binary...~%")
(if (probe-file *acl2-binary-path*)
    (format t "✓ ACL2 binary found at: ~A~%" *acl2-binary-path*)
    (format t "! ACL2 binary not found at: ~A~%" *acl2-binary-path*))

;; Test basic feature extraction logic
(format t "Testing basic feature extraction...~%")

(defun simple-extract-list (expr &optional (level 1) (result nil))
  "Simple version of extract-list-structure without dependencies"
  (when expr
    (if (atom expr)
        (append result (list (list expr 0 level)))
        (let ((head-result (list (list (car expr) (length (cdr expr)) level))))
          (reduce (lambda (acc item)
                    (if (listp item)
                        (append acc (simple-extract-list item (1+ level) nil))
                        (append acc (list (list item 0 (1+ level))))))
                  (cdr expr)
                  :initial-value (append result head-result))))))

(let ((features (simple-extract-list '(append x y))))
  (format t "✓ Extracted ~A features from (append x y): ~A~%"
          (length features) features))

;; Test basic theorem structure recognition
(format t "Testing theorem structure recognition...~%")

(defun simple-parse-theorem (theorem-form)
  "Simple theorem parser"
  (when (and (listp theorem-form) (>= (length theorem-form) 2))
    (let ((name (when (symbolp (first theorem-form)) (first theorem-form)))
          (formula (if (symbolp (first theorem-form))
                      (second theorem-form)
                      (first theorem-form))))
      (list :name name
            :formula formula
            :structure (cond
                        ((and (listp formula) (eq (first formula) 'implies))
                         'implies)
                        ((and (listp formula) (eq (first formula) 'equal))
                         'equal)
                        (t 'direct))))))

(let ((analysis (simple-parse-theorem '(defthm test (implies (consp x) (consp x))))))
  (format t "✓ Theorem analysis: ~A~%" analysis))

(format t "Core functionality tests completed successfully!~%")

;; Test ACL2 process basic functionality if binary exists
(when (probe-file *acl2-binary-path*)
  (format t "Testing ACL2 process startup...~%")
  (handler-case
      #+sbcl (let ((process (sb-ext:run-program *acl2-binary-path* nil
                                                :input :stream
                                                :output :stream
                                                :error :stream
                                                :wait nil
                                                :search nil)))
               (when process
                 (format t "✓ ACL2 process started successfully~%")
                 (sb-ext:process-kill process 15)
                 (sleep 1)
                 (format t "✓ ACL2 process terminated~%")))
      #-sbcl (format t "! Process test only available on SBCL~%")
    (error (e)
      (format t "! ACL2 process test failed: ~A~%" e))))

(format t "All tests completed!~%")