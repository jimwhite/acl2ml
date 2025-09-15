;;;; feature-extraction.lisp
;;;; Feature extraction from ACL2 expressions for machine learning
;;;; Ported from original ACL2(ml) Emacs Lisp code

(in-package #:acl2ml-mcp)

;;; Core feature extraction from S-expressions
(defstruct feature-vector
  symbols       ; List of symbols with arity and depth
  structure     ; Structural features (depth, breadth, etc.)
  complexity    ; Complexity metrics
  )

(defun extract-list-structure (expr &optional (level 1) (result nil))
  "Extract structural features from an S-expression
   Returns list of (symbol arity depth) tuples"
  (when expr
    (if (atom expr)
        ;; Atomic expression
        (append result (list (list expr 0 level)))
        ;; List expression
        (let ((head-result (list (list (car expr) (length (cdr expr)) level))))
          (reduce (lambda (acc item)
                    (if (listp item)
                        (append acc (extract-list-structure item (1+ level) nil))
                        (append acc (list (list item 0 (1+ level))))))
                  (cdr expr)
                  :initial-value (append result head-result))))))

(defun quicksort-features (feature-list sort-column)
  "Sort features by specified column (0=symbol, 1=arity, 2=depth)"
  (when (<= (length feature-list) 1)
    (return-from quicksort-features feature-list))

  (let ((pivot (nth sort-column (first feature-list)))
        (less nil)
        (equal nil)
        (greater nil))

    (dolist (feature feature-list)
      (let ((value (nth sort-column feature)))
        (cond
          ((< value pivot) (push feature less))
          ((= value pivot) (push feature equal))
          ((> value pivot) (push feature greater)))))

    (append (quicksort-features less sort-column)
            equal
            (quicksort-features greater sort-column))))

(defun normalize-arities (features)
  "Convert zero-arity atoms to arity -1 for consistency"
  (mapcar (lambda (feature)
            (let ((symbol (first feature))
                  (arity (second feature))
                  (depth (third feature)))
              (if (and (= arity 0) (not (numberp symbol)))
                  (list symbol -1 depth)
                  feature)))
          features))

(defun extract-features (expr)
  "Extract comprehensive features from ACL2 expression"
  (let* ((raw-features (extract-list-structure expr))
         (normalized-features (normalize-arities raw-features))
         (sorted-features (quicksort-features normalized-features 2))) ; Sort by depth

    (make-feature-vector
     :symbols sorted-features
     :structure (compute-structural-features expr)
     :complexity (compute-complexity-metrics expr))))

;;; Structural feature computation
(defun compute-structural-features (expr)
  "Compute structural metrics of expression"
  (let ((max-depth 0)
        (total-nodes 0)
        (leaf-nodes 0)
        (branching-factor 0))

    (labels ((traverse (node depth)
               (incf total-nodes)
               (setf max-depth (max max-depth depth))

               (if (atom node)
                   (incf leaf-nodes)
                   (let ((children (length (cdr node))))
                     (setf branching-factor (max branching-factor children))
                     (dolist (child (cdr node))
                       (traverse child (1+ depth)))))))

      (traverse expr 0)

      (list :max-depth max-depth
            :total-nodes total-nodes
            :leaf-nodes leaf-nodes
            :max-branching-factor branching-factor
            :avg-branching-factor (if (> total-nodes leaf-nodes)
                                      (/ (- total-nodes leaf-nodes)
                                         (- total-nodes leaf-nodes))
                                      0)))))

;;; Complexity metrics
(defun compute-complexity-metrics (expr)
  "Compute complexity metrics for ML features"
  (let ((symbol-count (make-hash-table :test 'eq))
        (function-calls 0)
        (unique-symbols 0))

    (labels ((count-symbols (node)
               (if (atom node)
                   (when (symbolp node)
                     (incf (gethash node symbol-count 0)))
                   (progn
                     (when (symbolp (car node))
                       (incf (gethash (car node) symbol-count 0))
                       (incf function-calls))
                     (dolist (child (cdr node))
                       (count-symbols child))))))

      (count-symbols expr)
      (setf unique-symbols (hash-table-count symbol-count))

      (list :total-symbols (reduce #'+ (loop for count being the hash-values of symbol-count collect count))
            :unique-symbols unique-symbols
            :function-calls function-calls
            :symbol-diversity (if (> function-calls 0)
                                  (/ unique-symbols function-calls)
                                  0)))))

;;; Feature vector operations for ML
(defun flatten-feature-vector (feature-vector)
  "Convert feature vector to flat list for ML algorithms"
  (let ((symbols (feature-vector-symbols feature-vector))
        (structure (feature-vector-structure feature-vector))
        (complexity (feature-vector-complexity feature-vector)))

    (append
     ;; Symbol-based features (first N most common symbols)
     (mapcar #'second (subseq symbols 0 (min 20 (length symbols))))
     ;; Structural features
     (list (getf structure :max-depth)
           (getf structure :total-nodes)
           (getf structure :leaf-nodes)
           (getf structure :max-branching-factor))
     ;; Complexity features
     (list (getf complexity :total-symbols)
           (getf complexity :unique-symbols)
           (getf complexity :function-calls)
           (getf complexity :symbol-diversity)))))

(defun feature-vector-to-csv-row (feature-vector)
  "Convert feature vector to CSV row string"
  (let ((flat-features (flatten-feature-vector feature-vector)))
    (format nil "~{~A~^,~}" flat-features)))

;;; Similarity computation
(defun compute-cosine-similarity (vec1 vec2)
  "Compute cosine similarity between two feature vectors"
  (let ((dot-product 0)
        (magnitude1 0)
        (magnitude2 0))

    (dotimes (i (min (length vec1) (length vec2)))
      (let ((v1 (nth i vec1))
            (v2 (nth i vec2)))
        (incf dot-product (* v1 v2))
        (incf magnitude1 (* v1 v1))
        (incf magnitude2 (* v2 v2))))

    (setf magnitude1 (sqrt magnitude1))
    (setf magnitude2 (sqrt magnitude2))

    (if (and (> magnitude1 0) (> magnitude2 0))
        (/ dot-product (* magnitude1 magnitude2))
        0)))

(defun compute-euclidean-distance (vec1 vec2)
  "Compute Euclidean distance between two feature vectors"
  (let ((sum-squares 0))
    (dotimes (i (min (length vec1) (length vec2)))
      (let ((diff (- (nth i vec1) (nth i vec2))))
        (incf sum-squares (* diff diff))))
    (sqrt sum-squares)))

(defun find-similar-expressions (target-expr expressions &key (threshold 0.8) (metric :cosine))
  "Find expressions similar to target using specified similarity metric"
  (let ((target-features (flatten-feature-vector (extract-features target-expr)))
        (results nil))

    (dolist (expr expressions)
      (let* ((expr-features (flatten-feature-vector (extract-features expr)))
             (similarity (case metric
                          (:cosine (compute-cosine-similarity target-features expr-features))
                          (:euclidean (- 1 (/ (compute-euclidean-distance target-features expr-features)
                                             (max 1 (length target-features)))))
                          (t (compute-cosine-similarity target-features expr-features)))))
        (when (>= similarity threshold)
          (push (list expr similarity) results))))

    (sort results (lambda (a b) (> (second a) (second b))))))

;;; Integration with definitions index
(defun extract-features-from-definition (definition)
  "Extract features from an ACL2 definition"
  (when (acl2-definition-body definition)
    (let ((features (extract-features (acl2-definition-body definition))))
      (setf (acl2-definition-features definition) features)
      features)))

(defun compute-definition-similarity (def1 def2)
  "Compute similarity between two definitions"
  (let ((features1 (or (acl2-definition-features def1)
                       (extract-features-from-definition def1)))
        (features2 (or (acl2-definition-features def2)
                       (extract-features-from-definition def2))))

    (when (and features1 features2)
      (compute-cosine-similarity
       (flatten-feature-vector features1)
       (flatten-feature-vector features2)))))

;;; Batch processing for ML dataset generation
(defun extract-all-definition-features ()
  "Extract features from all definitions in the index"
  (let ((processed-count 0))
    (maphash (lambda (name definition)
               (declare (ignore name))
               (extract-features-from-definition definition)
               (incf processed-count)
               (when (zerop (mod processed-count 100))
                 (format t "Processed ~A definitions...~%" processed-count)))
             *definitions-index*)
    processed-count))

(defun export-features-to-csv (filename)
  "Export all definition features to CSV file for ML training"
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream "name,type,book,~{feature~A~^,~}~%"
            (loop for i from 1 to 32 collect i)) ; Assuming 32 features

    (maphash (lambda (name definition)
               (declare (ignore name))
               (let ((features (or (acl2-definition-features definition)
                                   (extract-features-from-definition definition))))
                 (when features
                   (format stream "~A,~A,~A,~A~%"
                           (acl2-definition-name definition)
                           (acl2-definition-type definition)
                           (acl2-definition-book definition)
                           (feature-vector-to-csv-row features)))))
             *definitions-index*)))