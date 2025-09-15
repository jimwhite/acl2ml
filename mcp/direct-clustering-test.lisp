;; Direct clustering test - no MCP layer, just test functionality
:q

(load "~/quicklisp/setup.lisp")
(load "/workspaces/acl2ml/mcp/clustering.lisp")

;; Redirect output to stderr so stdout is clean
(setf *standard-output* *error-output*)

(format *error-output* "Loading definitions...~%")

;; Load definitions from index
(defparameter *test-defs* nil)
(with-open-file (stream "/workspaces/acl2ml/mcp/definitions-index.dat" :direction :input)
  (loop for line = (read-line stream nil)
        until (null line)
        unless (char= (char line 0) #\;)
        do (let ((def (ignore-errors (read-from-string line))))
             (when (and def (= (length def) 4))
               (push def *test-defs*)))))

(setf *test-defs* (nreverse *test-defs*))

(format *error-output* "Loaded ~A definitions~%" (length *test-defs*))

;; Test clustering
(in-package :acl2ml-clustering)

(format *error-output* "Running clustering...~%")

(let ((result (cluster-definitions *test-defs* :k-means :granularity-level 3)))
  (format *error-output* "Clustering complete~%")

  ;; Output results to stdout (clean)
  (let ((*standard-output* (make-synonym-stream '*terminal-io*)))
    (format t "~%CLUSTERING RESULTS FOR EXAMPLE.LISP~%")
    (format t "====================================~%")
    (format-clustering-results result *test-defs*)

    (format t "~%SIMILARITY TEST: THETA_SUM~%")
    (format t "===========================~%")
    (let ((similar (find-similar-items 'THETA_SUM *test-defs* :k-means)))
      (format-similarity-results '(THETA_SUM) similar))))

(sb-ext:exit)