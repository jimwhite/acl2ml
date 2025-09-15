;;;; analyze-example.lisp
;;;; Analyze manual/example.lisp and show clusters

(format t "ACL2(ml) - Analyzing manual/example.lisp for clusters...~%")
(format t "=========================================================~%")

;; Load our implementation
(load "package-minimal.lisp")
(in-package :acl2ml-mcp)

;; Set up paths
(defparameter *acl2-binary-path* "/home/acl2/saved_acl2")
(defparameter *acl2-books-dir* "/home/acl2/books/")

;; Load core functionality
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
  "Compute complexity metrics from features"
  (let ((total-nodes (length features))
        (max-depth (reduce #'max features :key #'third :initial-value 0))
        (unique-symbols (length (remove-duplicates features :key #'first :test #'equal)))
        (function-calls (count-if (lambda (f) (> (second f) 0)) features)))
    (list :total-nodes total-nodes
          :max-depth max-depth
          :unique-symbols unique-symbols
          :function-calls function-calls
          :complexity-score (+ (* max-depth 2) (* function-calls 3) unique-symbols))))

(defun flatten-feature-vector (features complexity)
  "Create flat feature vector for similarity comparison"
  (append
   ;; Structural features
   (list (getf complexity :total-nodes)
         (getf complexity :max-depth)
         (getf complexity :unique-symbols)
         (getf complexity :function-calls)
         (getf complexity :complexity-score))
   ;; Symbol frequency features (top 10 most common symbols)
   (let ((symbol-counts (make-hash-table :test 'equal)))
     (dolist (feature features)
       (incf (gethash (first feature) symbol-counts 0)))
     (mapcar #'cdr
             (subseq (sort (loop for symbol being the hash-keys of symbol-counts
                                 using (hash-value count)
                                 collect (cons count symbol))
                           #'> :key #'car)
                     0 (min 10 (hash-table-count symbol-counts)))))))

(defun cosine-similarity (vec1 vec2)
  "Compute cosine similarity between two vectors"
  (let ((dot-product 0)
        (mag1 0)
        (mag2 0))
    (dotimes (i (min (length vec1) (length vec2)))
      (let ((v1 (or (nth i vec1) 0))
            (v2 (or (nth i vec2) 0)))
        (incf dot-product (* v1 v2))
        (incf mag1 (* v1 v1))
        (incf mag2 (* v2 v2))))
    (if (and (> mag1 0) (> mag2 0))
        (/ dot-product (sqrt (* mag1 mag2)))
        0)))

;; Parse the example.lisp file
(format t "Reading and parsing manual/example.lisp...~%")

(defparameter *definitions* nil)

(with-open-file (stream "../manual/example.lisp" :direction :input)
  (loop for form = (read stream nil :eof)
        until (eq form :eof)
        when (and (listp form)
                  (member (first form) '(defun defthm)))
        do (push form *definitions*)))

(format t "Found ~A definitions~%~%" (length *definitions*))

;; Analyze each definition
(defparameter *analyzed-definitions* nil)

(dolist (def *definitions*)
  (let* ((type (first def))
         (name (second def))
         (body (if (eq type 'defun)
                  (fourth def)  ; function body
                  (third def))) ; theorem formula
         (features (extract-list-structure body))
         (complexity (compute-complexity features))
         (feature-vector (flatten-feature-vector features complexity)))

    (push (list :name name
                :type type
                :features features
                :complexity complexity
                :feature-vector feature-vector
                :definition def)
          *analyzed-definitions*)))

(setf *analyzed-definitions* (nreverse *analyzed-definitions*))

;; Display individual analysis
(format t "INDIVIDUAL DEFINITION ANALYSIS~%")
(format t "==============================~%")

(dolist (analysis *analyzed-definitions*)
  (format t "~A (~A)~%"
          (getf analysis :name)
          (getf analysis :type))
  (format t "  Complexity: ~A~%"
          (getf (getf analysis :complexity) :complexity-score))
  (format t "  Nodes: ~A, Depth: ~A, Functions: ~A~%"
          (getf (getf analysis :complexity) :total-nodes)
          (getf (getf analysis :complexity) :max-depth)
          (getf (getf analysis :complexity) :function-calls))
  (format t "  Feature Vector: [~{~A~^, ~}]~%~%"
          (subseq (getf analysis :feature-vector) 0
                  (min 8 (length (getf analysis :feature-vector))))))

;; Compute similarity matrix and perform clustering
(format t "~%SIMILARITY ANALYSIS & CLUSTERING~%")
(format t "=================================~%")

(defun compute-all-similarities ()
  "Compute similarity matrix between all definitions"
  (let ((similarities nil))
    (loop for i from 0 below (length *analyzed-definitions*)
          do (loop for j from (1+ i) below (length *analyzed-definitions*)
                   do (let* ((def1 (nth i *analyzed-definitions*))
                             (def2 (nth j *analyzed-definitions*))
                             (sim (cosine-similarity
                                   (getf def1 :feature-vector)
                                   (getf def2 :feature-vector))))
                        (push (list (getf def1 :name)
                                   (getf def2 :name)
                                   sim)
                              similarities))))
    (sort similarities #'> :key #'third)))

(defparameter *similarities* (compute-all-similarities))

;; Show top similarities
(format t "TOP SIMILARITIES:~%")
(dolist (sim (subseq *similarities* 0 (min 10 (length *similarities*))))
  (format t "  ~A ↔ ~A: ~,3F~%"
          (first sim) (second sim) (third sim)))

;; Simple clustering by similarity threshold
(format t "~%CLUSTERING (similarity ≥ 0.7):~%")

(defparameter *clusters* nil)
(defparameter *used-definitions* (make-hash-table :test 'equal))

(dolist (sim *similarities*)
  (when (>= (third sim) 0.7)
    (let ((def1 (first sim))
          (def2 (second sim)))
      (unless (or (gethash def1 *used-definitions*)
                  (gethash def2 *used-definitions*))
        (let ((cluster (list def1 def2)))
          ;; Add other similar definitions to this cluster
          (dolist (other-sim *similarities*)
            (when (and (>= (third other-sim) 0.6)
                      (or (equal (first other-sim) def1)
                          (equal (first other-sim) def2)
                          (equal (second other-sim) def1)
                          (equal (second other-sim) def2)))
              (let ((other-def (cond
                                ((equal (first other-sim) def1) (second other-sim))
                                ((equal (first other-sim) def2) (second other-sim))
                                ((equal (second other-sim) def1) (first other-sim))
                                ((equal (second other-sim) def2) (first other-sim)))))
                (when (and other-def (not (member other-def cluster :test #'equal)))
                  (push other-def cluster)))))
          (push cluster *clusters*)
          (dolist (def cluster)
            (setf (gethash def *used-definitions*) t)))))))

;; Display clusters
(if *clusters*
    (progn
      (loop for cluster in *clusters*
            for i from 1
            do (format t "Cluster ~A: ~{~A~^, ~}~%" i cluster))

      ;; Show cluster analysis
      (format t "~%CLUSTER ANALYSIS:~%")
      (loop for cluster in *clusters*
            for i from 1
            do (format t "~%Cluster ~A Analysis:~%" i)
               (let ((cluster-defs (mapcar (lambda (name)
                                            (find name *analyzed-definitions*
                                                  :key (lambda (x) (getf x :name))
                                                  :test #'equal))
                                          cluster)))
                 (format t "  Definitions: ~{~A~^, ~}~%" cluster)
                 (format t "  Types: ~{~A~^, ~}~%"
                         (remove-duplicates (mapcar (lambda (d) (getf d :type)) cluster-defs)))
                 (format t "  Avg Complexity: ~,2F~%"
                         (/ (reduce #'+ cluster-defs
                                   :key (lambda (d) (getf (getf d :complexity) :complexity-score)))
                            (length cluster-defs)))

                 ;; Show common patterns in cluster
                 (let ((all-features (apply #'append (mapcar (lambda (d) (getf d :features)) cluster-defs))))
                   (format t "  Common symbols: ~{~A~^, ~}~%"
                           (subseq (remove-duplicates (mapcar #'first all-features) :test #'equal)
                                  0 (min 5 (length (remove-duplicates (mapcar #'first all-features) :test #'equal)))))))))
    (format t "No strong clusters found (threshold = 0.7)~%"))

;; Show ungrouped definitions
(format t "~%UNGROUPED DEFINITIONS:~%")
(dolist (analysis *analyzed-definitions*)
  (unless (gethash (getf analysis :name) *used-definitions*)
    (format t "  ~A (~A) - complexity: ~A~%"
            (getf analysis :name)
            (getf analysis :type)
            (getf (getf analysis :complexity) :complexity-score))))

(format t "~%Analysis complete!~%")