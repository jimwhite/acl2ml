;;;; test-core.lisp
;;;; Test core ACL2(ml) functionality without MCP dependencies

(format t "Testing ACL2(ml) core functionality with standard SBCL...~%")

;; Load minimal package
(load "package-minimal.lisp")
(format t "✓ Package loaded~%")

(in-package :acl2ml-mcp)

;; Test configuration
(defparameter *acl2-binary-path* "/home/acl2/saved_acl2")
(defparameter *acl2-books-dir* "/home/acl2/books/")
(defparameter *definitions-index* (make-hash-table :test 'equal))

(format t "Configuration:~%")
(format t "  ACL2 Binary: ~A~%" *acl2-binary-path*)
(format t "  ACL2 Books: ~A~%" *acl2-books-dir*)

;; Check file existence
(if (probe-file *acl2-binary-path*)
    (format t "✓ ACL2 binary found~%")
    (format t "✗ ACL2 binary not found~%"))

(if (probe-file *acl2-books-dir*)
    (format t "✓ ACL2 books directory found~%")
    (format t "✗ ACL2 books directory not found~%"))

;; Test basic feature extraction
(format t "~%Testing feature extraction...~%")

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

(let ((features (extract-list-structure '(implies (and (consp x) (consp y))
                                                  (equal (append x y) (cons x y))))))
  (format t "✓ Extracted ~A features from complex expression~%" (length features))
  (format t "  Features: ~A~%" (subseq features 0 (min 5 (length features)))))

;; Test theorem analysis
(format t "~%Testing theorem analysis...~%")

(defstruct theorem-info
  name
  type
  hypothesis
  conclusion
  complexity)

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
         :name name
         :type type
         :hypothesis hyp
         :conclusion concl
         :complexity (length (extract-list-structure formula)))))))

(let ((analysis (analyze-theorem-structure
                 '(defthm append-associative
                    (implies (and (true-listp x) (true-listp y))
                             (equal (append (append x y) z)
                                   (append x (append y z))))))))
  (format t "✓ Theorem analysis completed~%")
  (format t "  Name: ~A~%" (theorem-info-name analysis))
  (format t "  Type: ~A~%" (theorem-info-type analysis))
  (format t "  Complexity: ~A features~%" (theorem-info-complexity analysis)))

;; Test basic ACL2 process interaction
(format t "~%Testing ACL2 process interaction...~%")

(when (probe-file *acl2-binary-path*)
  (handler-case
      (let ((process #+sbcl (sb-ext:run-program *acl2-binary-path* nil
                                                :input :stream
                                                :output :stream
                                                :error :stream
                                                :wait nil)
                     #-sbcl nil))
        (if process
            (progn
              (format t "✓ ACL2 process started~%")

              ;; Try to read initial output
              #+sbcl
              (let ((output-stream (sb-ext:process-output process)))
                (sleep 2) ; Wait for ACL2 to start
                (when (listen output-stream)
                  (let ((initial-output (make-string 1000 :initial-element #\Space)))
                    (handler-case
                        (let ((chars-read (read-sequence initial-output output-stream)))
                          (format t "✓ Read ~A characters from ACL2~%" chars-read)
                          (format t "  Sample output: ~A~%"
                                  (string-trim '(#\Space #\Newline #\Null)
                                              (subseq initial-output 0 (min 100 chars-read)))))
                      (error (e)
                        (format t "! Error reading ACL2 output: ~A~%" e)))))

              ;; Clean up
              #+sbcl (sb-ext:process-kill process 15)
              (sleep 1)
              (format t "✓ ACL2 process terminated~%"))
            (format t "✗ Failed to start ACL2 process~%")))
    (error (e)
      (format t "✗ ACL2 process test failed: ~A~%" e)))))

(format t "~%Core functionality tests completed!~%")