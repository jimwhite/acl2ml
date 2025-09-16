:q
(load "~/quicklisp/setup.lisp")
(ql:quickload :jsonrpc :silent t)
(ql:quickload "jsonrpc/transport/stdio" :silent t)
(ql:quickload :40ants-mcp :silent t)
(load "/workspaces/acl2ml/mcp/clustering.lisp")
(load "/workspaces/acl2ml/mcp/feature-extraction-full.lisp")

(format t "~%=== ACL2(ml) Direct Test ===~%")

;; Test the clustering functionality directly
(in-package :acl2ml-clustering)

;; Load definitions
(defparameter *test-definitions*
  '((THETA_SUM DEFUN "/workspaces/acl2ml/manual/example.lisp" 75)
    (HELPER_SUM DEFUN "/workspaces/acl2ml/manual/example.lisp" 81)
    (FN_SUM DEFUN "/workspaces/acl2ml/manual/example.lisp" 35)
    (THETA_FACT DEFUN "/workspaces/acl2ml/manual/example.lisp" 77)
    (HELPER_FACT DEFUN "/workspaces/acl2ml/manual/example.lisp" 83)
    (FN_FACT DEFUN "/workspaces/acl2ml/manual/example.lisp" 37)))

(format t "Testing clustering with ~A definitions...~%" (length *test-definitions*))

;; Test clustering
(let ((result (cluster-definitions *test-definitions* :k-means :granularity-level 3)))
  (format t "Clustering result: ~A clusters found~%"
          (length (cluster-result-clusters result)))

  ;; Show clusters
  (format-clustering-results result *test-definitions*)

  ;; Test similarity search
  (format t "~%Testing similarity search for THETA_SUM...~%")
  (let ((similar (find-similar-items 'THETA_SUM *test-definitions* :k-means)))
    (format t "Found ~A similar items~%" (length similar))
    (format-similarity-results '(THETA_SUM) similar)))

;; Test feature extraction
(in-package :acl2ml-features)
(format t "~%Testing feature extraction...~%")

(let* ((expr '(implies (consp x) (equal (append x y) (foo x y))))
       (features (extract-features-full expr)))
  (format t "Expression: ~A~%" expr)
  (format t "Features extracted: ~A~%" (length features))
  (format t "Feature details: ~A~%" features))

(format t "~%=== Test Complete ===~%")
(sb-ext:exit)
