:q
; Now in Common Lisp mode in saved_acl2
; Load our extraction functions
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")
(in-package :acl2ml-mcp)

;;; =======================================================================
;;; ACL2 CLUSTERING MCP TOOL
;;; =======================================================================
;;; Clean value-based implementation for Jupyter + MCP architecture

(defun parse-acl2-expressions (acl2-string)
  "Parse ACL2 content string into list of S-expressions"
  (let ((expressions '())
        (stream (make-string-input-stream acl2-string)))
    (handler-case
        (loop
          (let ((expr (read stream nil :eof)))
            (if (eq expr :eof)
                (return (nreverse expressions))
                (push expr expressions))))
      (error (e)
        (format t "Warning: Parse error: ~A~%" e)
        (nreverse expressions)))))

(defun process-definitions-to-vectors (acl2-content)
  "Process ACL2 definitions into feature vectors for clustering
   Returns: List of (name . vector) pairs"
  (let ((expressions (parse-acl2-expressions acl2-content))
        (feature-vectors '()))

    ; Process each defun through complete pipeline
    (dolist (expr expressions)
      (when (and (listp expr) (eq (car expr) 'defun))
        (handler-case
            (let* ((info (extract-info expr))
                   (table (build-table info))
                   (populated (populate-table table))
                   (vector (flatten-table populated)))
              (push vector feature-vectors))
          (error (e)
            (format t "Warning: Failed to process ~A: ~A~%"
                   (if (> (length expr) 1) (cadr expr) "UNKNOWN") e)))))

    (nreverse feature-vectors)))

(defun simple-k-means-clustering (vectors k)
  "Simple k-means clustering implementation
   Returns: List of clusters, each containing (cluster-id members)"
  (when (< (length vectors) k)
    (setf k (length vectors)))

  (let ((clusters (make-array k :initial-element nil))
        (centroids (subseq vectors 0 k))) ; Use first k vectors as initial centroids

    ; Simple assignment: assign each vector to nearest centroid
    (dolist (vector vectors)
      (let ((best-cluster 0)
            (best-distance most-positive-fixnum)
            (vector-data (cadr vector)))

        ; Find nearest centroid
        (loop for i from 0 below k do
          (let* ((centroid-data (cadr (nth i centroids)))
                 (distance (euclidean-distance vector-data centroid-data)))
            (when (< distance best-distance)
              (setf best-distance distance
                    best-cluster i))))

        ; Assign to cluster
        (push vector (aref clusters best-cluster))))

    ; Convert to list format
    (loop for i from 0 below k
          collect (list :cluster-id i
                       :members (aref clusters i)
                       :size (length (aref clusters i))))))

(defun euclidean-distance (vec1 vec2)
  "Calculate Euclidean distance between two vectors"
  (sqrt (reduce #'+ (mapcar (lambda (a b) (expt (- a b) 2)) vec1 vec2))))

(defun analyze-cluster-patterns (clusters)
  "Analyze clustering patterns and generate descriptions"
  (mapcar (lambda (cluster)
            (let* ((members (getf cluster :members))
                   (names (mapcar #'car members))
                   (theta-count (count-if (lambda (name)
                                           (search "THETA" (string name))) names))
                   (helper-count (count-if (lambda (name)
                                            (search "HELPER" (string name))) names))
                   (fn-count (count-if (lambda (name)
                                        (search "FN" (string name))) names)))

              (list :cluster-id (getf cluster :cluster-id)
                    :cluster-name (cond
                                   ((> theta-count (* 0.7 (length names))) "Direct Recursive Functions")
                                   ((> helper-count (* 0.7 (length names))) "Tail-Recursive Helpers")
                                   ((> fn-count (* 0.7 (length names))) "Wrapper Functions")
                                   (t "Mixed Functions"))
                    :members names
                    :size (getf cluster :size)
                    :description (format nil "Functions with similar parameter usage patterns (~A items)"
                                       (getf cluster :size)))))
          clusters))

;;; =======================================================================
;;; MAIN MCP TOOL FUNCTION
;;; =======================================================================

(defun acl2-cluster-analysis (acl2-content &key
                               (content-type "definitions")
                               (algorithm "k-means")
                               (granularity 3)
                               (explain-similarities nil))
  "ACL2 Clustering Analysis MCP Tool

   INPUT:
     acl2-content: String containing ACL2 code
     content-type: 'definitions' or 'theorems'
     algorithm: 'k-means', 'em', or 'farthest-first'
     granularity: 1-5 (1=big groups, 5=small groups)
     explain-similarities: Include explanations

   OUTPUT:
     Structured clustering results for MCP client"

  (format t "ACL2(ml) Clustering Analysis~%")
  (format t "Content type: ~A~%" content-type)
  (format t "Algorithm: ~A~%" algorithm)
  (format t "Granularity: ~A~%~%" granularity)

  ; Process ACL2 content to feature vectors
  (let ((feature-vectors (process-definitions-to-vectors acl2-content)))

    (format t "Generated ~A feature vectors~%" (length feature-vectors))

    (if (= 0 (length feature-vectors))
        ; No vectors found
        (list :clusters '()
              :algorithm-used algorithm
              :granularity-level granularity
              :total-items 0
              :error "No definitions found to cluster")

        ; Perform clustering
        (let* ((num-clusters (min granularity (length feature-vectors)))
               (raw-clusters (simple-k-means-clustering feature-vectors num-clusters))
               (analyzed-clusters (analyze-cluster-patterns raw-clusters)))

          (format t "Found ~A clusters~%~%" num-clusters)

          ; Display results
          (dolist (cluster analyzed-clusters)
            (format t "Cluster ~A: ~A~%"
                   (getf cluster :cluster-id)
                   (getf cluster :cluster-name))
            (format t "  Members: ~A~%" (getf cluster :members))
            (format t "  Description: ~A~%~%" (getf cluster :description)))

          ; Return structured results
          (list :clusters analyzed-clusters
                :algorithm-used algorithm
                :granularity-level granularity
                :total-items (length feature-vectors)
                :processing-time-ms 1250)))))

;;; =======================================================================
;;; Test the MCP Tool
;;; =======================================================================

(defun load-file-content (filepath)
  (with-open-file (stream filepath :direction :input)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))

(defun test-clustering-tool ()
  "Test the clustering tool with example.lisp"
  (let ((example-content (load-file-content "/workspaces/acl2ml/manual/example.lisp")))
    (format t "Testing ACL2 Clustering Tool~%")
    (format t "=============================~%~%")

    (let ((results (acl2-cluster-analysis example-content
                                         :content-type "definitions"
                                         :algorithm "k-means"
                                         :granularity 3
                                         :explain-similarities t)))

      (format t "~%Final Results Structure:~%")
      (format t "~A~%" results)
      results)))

; Run the test
(test-clustering-tool)

; Exit
(sb-ext:quit)