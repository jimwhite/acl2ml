;;;; test-basic.lisp
;;;; Basic functionality test without ASDF

(format t "Testing ACL2(ml) components directly...~%")

;; Load files in dependency order
(load "package.lisp")
(format t "✓ Package loaded~%")

(load "acl2-interface.lisp")
(format t "✓ ACL2 interface loaded~%")

(load "definitions-index.lisp")
(format t "✓ Definitions index loaded~%")

(load "feature-extraction.lisp")
(format t "✓ Feature extraction loaded~%")

(load "lemma-analysis.lisp")
(format t "✓ Lemma analysis loaded~%")

;; Test basic functionality in the package
(in-package :acl2ml-mcp)

;; Test feature extraction
(format t "Testing feature extraction...~%")
(let ((features (extract-features '(append x y))))
  (format t "✓ Extracted ~A features from (append x y)~%"
          (length (feature-vector-symbols features))))

;; Test theorem analysis
(format t "Testing theorem analysis...~%")
(let ((analysis (analyze-theorem-structure
                 '(defthm test-thm (implies (consp x) (consp x))))))
  (format t "✓ Theorem structure: ~A~%"
          (theorem-analysis-logical-structure analysis)))

;; Test ACL2 binary check
(format t "Testing ACL2 binary path...~%")
(if (probe-file *acl2-binary-path*)
    (format t "✓ ACL2 binary found at: ~A~%" *acl2-binary-path*)
    (format t "! ACL2 binary not found at: ~A~%" *acl2-binary-path*))

(format t "Basic component tests completed successfully!~%")