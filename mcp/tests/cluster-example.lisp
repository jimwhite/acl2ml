;;;; cluster-example.lisp
;;;; Show clustering results for manual/example.lisp

(format t "ACL2(ml) Clustering Analysis for manual/example.lisp~%")
(format t "====================================================~%")

;; Load basic functionality
(load "package-minimal.lisp")
(in-package :acl2ml-mcp)

;; Feature extraction
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

(defun compute-complexity (features)
  "Compute complexity score"
  (let ((depth (reduce #'max features :key #'third :initial-value 0))
        (nodes (length features))
        (functions (count-if (lambda (f) (> (second f) 0)) features)))
    (+ nodes (* depth 2) (* functions 3))))

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

;; Parse example.lisp
(defparameter *definitions* nil)

(with-open-file (stream "../manual/example.lisp" :direction :input)
  (loop for form = (read stream nil :eof)
        until (eq form :eof)
        when (and (listp form) (member (first form) '(defun defthm)))
        do (push form *definitions*)))

(setf *definitions* (nreverse *definitions*))

(format t "Found ~A definitions in example.lisp~%~%" (length *definitions*))

;; Analyze each definition
(defparameter *analyzed* nil)

(dolist (def *definitions*)
  (let* ((type (first def))
         (name (second def))
         (body (if (eq type 'defun) (fourth def) (third def)))
         (features (extract-list-structure body))
         (complexity (compute-complexity features))
         (feature-vector (list complexity (length features)
                              (reduce #'max features :key #'third :initial-value 0))))

    (push (list :name name :type type :complexity complexity
                :features (length features) :depth (third feature-vector)
                :vector feature-vector :definition def)
          *analyzed*)))

(setf *analyzed* (nreverse *analyzed*))

;; Show individual analysis
(format t "INDIVIDUAL ANALYSIS~%")
(format t "==================~%")
(dolist (item *analyzed*)
  (format t "~A (~A): complexity=~A, features=~A, depth=~A~%"
          (getf item :name) (getf item :type) (getf item :complexity)
          (getf item :features) (getf item :depth)))

;; Compute similarities and cluster
(format t "~%SIMILARITY MATRIX~%")
(format t "=================~%")

(defparameter *similarities* nil)

(loop for i from 0 below (length *analyzed*)
      do (loop for j from (1+ i) below (length *analyzed*)
               do (let* ((item1 (nth i *analyzed*))
                         (item2 (nth j *analyzed*))
                         (sim (cosine-similarity (getf item1 :vector) (getf item2 :vector))))
                    (push (list (getf item1 :name) (getf item2 :name) sim) *similarities*))))

(setf *similarities* (sort *similarities* #'> :key #'third))

;; Show top similarities
(format t "Top 10 most similar pairs:~%")
(dolist (sim (subseq *similarities* 0 (min 10 (length *similarities*))))
  (format t "  ~A ↔ ~A: ~,3F~%" (first sim) (second sim) (third sim)))

;; Simple clustering by pattern matching
(format t "~%CLUSTERING BY FUNCTION PATTERNS~%")
(format t "===============================~%")

;; Group by naming pattern
(defparameter *clusters* (make-hash-table :test 'equal))

(dolist (item *analyzed*)
  (let* ((name (symbol-name (getf item :name)))
         (pattern (cond
                   ((search "THETA_" name) "theta-functions")
                   ((search "HELPER_" name) "helper-functions")
                   ((search "FN_" name) "wrapper-functions")
                   ((search "_IS_" name) "equivalence-theorems")
                   (t "other"))))
    (push item (gethash pattern *clusters*))))

;; Display clusters
(maphash (lambda (pattern items)
           (format t "~%Cluster: ~A (~A items)~%" pattern (length items))
           (format t "Members: ~{~A~^, ~}~%"
                   (mapcar (lambda (item) (getf item :name)) items))
           (when (> (length items) 1)
             (let ((avg-complexity (/ (reduce #'+ items :key (lambda (x) (getf x :complexity)))
                                      (length items))))
               (format t "Average complexity: ~,1F~%" avg-complexity))))
         *clusters*)

;; Functional clustering by similarity
(format t "~%SIMILARITY-BASED CLUSTERING~%")
(format t "===========================~%")

(defparameter *sim-clusters* nil)
(defparameter *used* (make-hash-table :test 'equal))

(dolist (sim *similarities*)
  (when (>= (third sim) 0.8)  ; High similarity threshold
    (let ((name1 (first sim))
          (name2 (second sim)))
      (unless (or (gethash name1 *used*) (gethash name2 *used*))
        (let ((cluster (list name1 name2)))
          ;; Find other similar functions
          (dolist (other-sim *similarities*)
            (when (and (>= (third other-sim) 0.7)
                      (or (eq (first other-sim) name1)
                          (eq (first other-sim) name2)
                          (eq (second other-sim) name1)
                          (eq (second other-sim) name2)))
              (let ((other-name (cond
                                 ((eq (first other-sim) name1) (second other-sim))
                                 ((eq (first other-sim) name2) (second other-sim))
                                 ((eq (second other-sim) name1) (first other-sim))
                                 ((eq (second other-sim) name2) (first other-sim)))))
                (when (and other-name (not (member other-name cluster)))
                  (push other-name cluster)))))
          (push cluster *sim-clusters*)
          (dolist (name cluster)
            (setf (gethash name *used*) t)))))))

(if *sim-clusters*
    (loop for cluster in *sim-clusters*
          for i from 1
          do (format t "Similarity Cluster ~A: ~{~A~^, ~}~%" i cluster))
    (format t "No high-similarity clusters found (threshold >= 0.8)~%"))

(format t "~%ANALYSIS COMPLETE~%")
(format t "Results show function groupings by:~%")
(format t "1. Naming patterns (theta_, helper_, fn_, theorems)~%")
(format t "2. Structural similarity (complexity, depth, features)~%")

(sb-ext:exit)