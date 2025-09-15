;;;; test-system.lisp
;;;; Quick test of ACL2(ml) MCP system

(format t "Loading ACL2(ml) MCP system...~%")

;; Try to load the system
(handler-case
    (progn
      (asdf:load-system :acl2ml-mcp :verbose t)
      (format t "✓ System loaded successfully~%"))
  (error (e)
    (format t "✗ Error loading system: ~A~%" e)
    (sb-ext:exit :code 1)))

;; Test basic functionality
(in-package :acl2ml-mcp)

(format t "Testing basic functionality...~%")

;; Test definitions index
(handler-case
    (progn
      (load-definitions-index)
      (format t "✓ Definitions index loading works~%"))
  (error (e)
    (format t "! Definitions index: ~A~%" e)))

;; Test feature extraction
(handler-case
    (let ((features (extract-features '(append x y))))
      (format t "✓ Feature extraction works: ~A features~%"
              (length (flatten-feature-vector features))))
  (error (e)
    (format t "✗ Feature extraction failed: ~A~%" e)))

;; Test theorem analysis
(handler-case
    (let ((analysis (analyze-theorem-structure
                     '(defthm test-thm (implies (consp x) (consp x))))))
      (format t "✓ Theorem analysis works: ~A~%"
              (theorem-analysis-logical-structure analysis)))
  (error (e)
    (format t "✗ Theorem analysis failed: ~A~%" e)))

(format t "Basic tests completed~%")