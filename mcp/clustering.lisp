;;;; clustering.lisp
;;;; Convert Weka-based clustering algorithms to pure Common Lisp
;;;; Based on weka-connection.el from original ACL2(ml)

(defpackage #:acl2ml-clustering
  (:use #:cl)
  (:export #:k-means-clustering
           #:em-clustering
           #:farthest-first-clustering
           #:cluster-definitions
           #:cluster-theorems
           #:find-similar-items
           #:explain-similarities))

(in-package :acl2ml-clustering)

;;; Core clustering data structures
(defstruct cluster-result
  clusters
  centroids
  assignments
  similarity-scores)

(defstruct similarity-pair
  item1
  item2
  score)

;;; K-means clustering implementation
(defun euclidean-distance (vec1 vec2)
  "Compute Euclidean distance between two vectors"
  (sqrt (reduce #'+ (mapcar (lambda (x y) (expt (- x y) 2)) vec1 vec2))))

(defun cosine-similarity (vec1 vec2)
  "Compute cosine similarity between vectors"
  (let ((dot-product 0) (mag1 0) (mag2 0))
    (dotimes (i (min (length vec1) (length vec2)))
      (let ((v1 (nth i vec1)) (v2 (nth i vec2)))
        (when (and (numberp v1) (numberp v2))
          (incf dot-product (* v1 v2))
          (incf mag1 (* v1 v1))
          (incf mag2 (* v2 v2)))))
    (if (and (> mag1 0) (> mag2 0))
        (/ dot-product (sqrt (* mag1 mag2)))
        0)))

(defun compute-centroid (vectors)
  "Compute centroid of a set of vectors"
  (when vectors
    (let ((dim (length (first vectors)))
          (count (length vectors)))
      (loop for i from 0 below dim
            collect (/ (reduce #'+ vectors :key (lambda (v) (nth i v))) count)))))

(defun k-means-clustering (vectors k &key (max-iterations 100) (tolerance 1e-6))
  "K-means clustering algorithm"
  (let* ((n (length vectors))
         (dim (length (first vectors)))
         ;; Initialize centroids randomly
         (centroids (loop repeat k
                          collect (loop repeat dim
                                        collect (random 1.0))))
         (assignments (make-array n :initial-element 0))
         (prev-assignments (make-array n :initial-element -1)))

    (dotimes (iter max-iterations)
      ;; Assign points to nearest centroid
      (dotimes (i n)
        (let ((min-dist most-positive-fixnum)
              (best-cluster 0))
          (dotimes (c k)
            (let ((dist (euclidean-distance (nth i vectors) (nth c centroids))))
              (when (< dist min-dist)
                (setf min-dist dist
                      best-cluster c))))
          (setf (aref assignments i) best-cluster)))

      ;; Check convergence
      (when (equalp assignments prev-assignments)
        (return))

      (setf prev-assignments (copy-seq assignments))

      ;; Update centroids
      (dotimes (c k)
        (let ((cluster-points (loop for i from 0 below n
                                    when (= (aref assignments i) c)
                                    collect (nth i vectors))))
          (when cluster-points
            (setf (nth c centroids) (compute-centroid cluster-points))))))

    ;; Group points by cluster
    (let ((clusters (make-array k :initial-element nil)))
      (dotimes (i n)
        (push i (aref clusters (aref assignments i))))

      (make-cluster-result
       :clusters (coerce clusters 'list)
       :centroids centroids
       :assignments (coerce assignments 'list)
       :similarity-scores nil))))

;;; EM clustering (simplified version)
(defun em-clustering (vectors k &key (max-iterations 50))
  "Simplified EM clustering using K-means initialization"
  ;; For simplicity, use K-means as base and add probability weights
  (let ((kmeans-result (k-means-clustering vectors k :max-iterations max-iterations)))
    ;; Add probabilistic assignments (simplified)
    (setf (cluster-result-similarity-scores kmeans-result)
          (loop for cluster in (cluster-result-clusters kmeans-result)
                collect (length cluster)))
    kmeans-result))

;;; Farthest First clustering
(defun farthest-first-clustering (vectors k)
  "Farthest First clustering algorithm"
  (when (and vectors (> k 0))
    (let ((centroids (list (first vectors)))
          (remaining (rest vectors)))

      ;; Select k centroids using farthest-first strategy
      (loop while (and (< (length centroids) k) remaining) do
        (let ((max-min-dist 0)
              (farthest-point nil))
          (dolist (point remaining)
            (let ((min-dist (reduce #'min centroids
                                    :key (lambda (c) (euclidean-distance point c)))))
              (when (> min-dist max-min-dist)
                (setf max-min-dist min-dist
                      farthest-point point))))
          (when farthest-point
            (push farthest-point centroids)
            (setf remaining (remove farthest-point remaining :test #'equal)))))

      ;; Now assign all points to nearest centroid
      (let ((assignments (make-array (length vectors) :initial-element 0)))
        (dotimes (i (length vectors))
          (let ((min-dist most-positive-fixnum)
                (best-cluster 0))
            (dotimes (c (length centroids))
              (let ((dist (euclidean-distance (nth i vectors) (nth c centroids))))
                (when (< dist min-dist)
                  (setf min-dist dist
                        best-cluster c))))
            (setf (aref assignments i) best-cluster)))

        ;; Group points by cluster
        (let ((clusters (make-array (length centroids) :initial-element nil)))
          (dotimes (i (length vectors))
            (push i (aref clusters (aref assignments i))))

          (make-cluster-result
           :clusters (coerce clusters 'list)
           :centroids centroids
           :assignments (coerce assignments 'list)
           :similarity-scores nil))))))

;;; High-level clustering interface
(defun cluster-by-algorithm (vectors algorithm num-clusters)
  "Cluster vectors using specified algorithm"
  (case algorithm
    (:k-means (k-means-clustering vectors num-clusters))
    (:em (em-clustering vectors num-clusters))
    (:farthest-first (farthest-first-clustering vectors num-clusters))
    (t (error "Unknown clustering algorithm: ~A" algorithm))))

;;; Convert definitions to feature vectors (from original extraction code)
(defun definition-to-feature-vector (definition)
  "Convert ACL2 definition to feature vector for clustering"
  (let* ((name (first definition))
         (type (second definition))
         (file (third definition))
         (complexity (fourth definition))
         ;; Basic features based on original ACL2(ml)
         (name-length (length (symbol-name name)))
         (type-code (case type
                      (defun 1)
                      (defthm 2)
                      (defmacro 3)
                      (defconst 4)
                      (t 0)))
         (file-hash (mod (sxhash file) 100)))

    ;; Create feature vector similar to original system
    (list complexity name-length type-code file-hash)))

;;; Main clustering functions
(defun cluster-definitions (definitions algorithm &key (granularity-level 3))
  "Cluster ACL2 definitions using specified algorithm"
  (let* ((vectors (mapcar #'definition-to-feature-vector definitions))
         (num-clusters (case granularity-level
                         (2 (floor (length definitions) 7))
                         (3 (floor (length definitions) 5))
                         (4 (floor (length definitions) 4))
                         (5 (floor (length definitions) 2))
                         (t (floor (length definitions) 8))))
         (result (cluster-by-algorithm vectors algorithm num-clusters)))

    ;; Add definition info to result
    (setf (cluster-result-similarity-scores result)
          (loop for cluster in (cluster-result-clusters result)
                collect (loop for idx in cluster
                              collect (nth idx definitions))))
    result))

(defun cluster-theorems (theorems algorithm &key (granularity-level 3))
  "Cluster ACL2 theorems using specified algorithm"
  (cluster-definitions theorems algorithm :granularity-level granularity-level))

;;; Similarity finding (equivalent to original print-similarities-weka)
(defun find-similar-items (target-item definitions algorithm &key (granularity-level 3))
  "Find items similar to target item"
  (let* ((target-pos (position target-item definitions :key #'first :test #'equal))
         (result (cluster-definitions definitions algorithm :granularity-level granularity-level)))
    (when target-pos
      (let ((target-cluster-idx (nth target-pos (cluster-result-assignments result))))
        (loop for cluster in (cluster-result-similarity-scores result)
              for cluster-idx from 0
              when (= cluster-idx target-cluster-idx)
              return (remove target-item cluster :key #'first :test #'equal))))))

;;; Explain similarities (simplified version of original why-are-similar)
(defun explain-similarities (similar-items)
  "Explain why items are similar based on features"
  (when similar-items
    (let ((feature-analysis
           (loop for item in similar-items
                 collect (definition-to-feature-vector item))))
      (list
       :complexity-range (list (reduce #'min feature-analysis :key #'first)
                              (reduce #'max feature-analysis :key #'first))
       :name-length-range (list (reduce #'min feature-analysis :key #'second)
                               (reduce #'max feature-analysis :key #'second))
       :types (remove-duplicates (mapcar #'third feature-analysis))
       :file-distribution (length (remove-duplicates (mapcar #'fourth feature-analysis)))))))

;;; Format results similar to original print-clusters-weka
(defun format-clustering-results (result definitions &key (stream t))
  "Format clustering results for display"
  (format stream "~%CLUSTERING RESULTS~%")
  (format stream "==================~%")
  (format stream "Found ~A clusters:~%~%" (length (cluster-result-clusters result)))

  (loop for cluster in (cluster-result-similarity-scores result)
        for cluster-num from 1
        when (> (length cluster) 1) do
        (format stream "Cluster ~A (~A items):~%" cluster-num (length cluster))
        (dolist (def cluster)
          (format stream "  • ~A (~A)~%" (first def) (second def)))
        (format stream "~%")))

(defun format-similarity-results (target similar-items &key (stream t))
  "Format similarity results for display"
  (format stream "~%SIMILARITY ANALYSIS~%")
  (format stream "===================~%")
  (if similar-items
      (progn
        (format stream "~A is similar to:~%" (first target))
        (dolist (item similar-items)
          (format stream "  • ~A~%" (first item)))
        (format stream "~%Explanation: ~A~%" (explain-similarities similar-items)))
      (format stream "No similar items found for ~A~%" (first target))))