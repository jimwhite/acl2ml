;;;; test-arithmetic-clustering.lisp
;;;; Test clustering on converted arithmetic book definitions

(format t "ACL2(ml) Clustering Analysis for Converted Arithmetic Books~%")
(format t "=========================================================~%")

;; Load basic functionality
(load "package-minimal.lisp")
(in-package :acl2ml-mcp)

;; Read our converted definitions
(defun load-definitions-from-file (file-path)
  "Load definitions from our converted format"
  (with-open-file (stream file-path :direction :input)
    (read stream)))

;; Load arithmetic book definitions
(defparameter *arithmetic-defs*
  (append
    (load-definitions-from-file "definitions/arithmetic-3_bind-free_arithmetic-theory")
    (load-definitions-from-file "definitions/arithmetic-3_pass1_basic-arithmetic")))

(format t "Loaded ~A arithmetic definitions~%~%" (length *arithmetic-defs*))

;; Display the definitions
(format t "ARITHMETIC DEFINITIONS~%")
(format t "=====================~%")
(dolist (def *arithmetic-defs*)
  (let ((name-path (car def))
        (features (cadr def)))
    ;; Extract function name from path::name format
    (let* ((name-str (symbol-name name-path))
           (colon-pos (search "::" name-str))
           (function-name (if colon-pos
                            (subseq name-str (+ colon-pos 2))
                            name-str)))
      (format t "~A: ~A features~%" function-name (count-if-not #'null (flatten-list features))))))

;; Helper to flatten nested lists
(defun flatten-list (lst)
  "Flatten nested list structure"
  (cond ((null lst) nil)
        ((atom lst) (list lst))
        (t (append (flatten-list (car lst)) (flatten-list (cdr lst))))))

;; Convert feature matrix to numeric vector
(defun feature-matrix-to-vector (matrix)
  "Convert 7x7 feature matrix to numeric vector"
  (let ((flattened (flatten-list matrix))
        (vector (make-array 49 :initial-element 0)))
    (loop for item in flattened
          for i from 0
          when (and item (< i 49))
          do (setf (aref vector i)
                   (cond ((numberp item) item)
                         ((symbolp item) (mod (sxhash item) 100))
                         (t 1))))
    (coerce vector 'list)))

;; Analyze each definition
(defparameter *analyzed-arithmetic* nil)

(dolist (def *arithmetic-defs*)
  (let* ((name-path (car def))
         (features (cadr def))
         (name-str (symbol-name name-path))
         (colon-pos (search "::" name-str))
         (function-name (if colon-pos
                          (subseq name-str (+ colon-pos 2))
                          name-str))
         (feature-vector (feature-matrix-to-vector features))
         (complexity (reduce #'+ feature-vector))
         (non-zero-count (count-if-not #'zerop feature-vector)))

    (push (list :name function-name :complexity complexity
                :non-zero-features non-zero-count
                :vector feature-vector :full-name name-path)
          *analyzed-arithmetic*)))

(setf *analyzed-arithmetic* (nreverse *analyzed-arithmetic*))

;; Show individual analysis
(format t "~%INDIVIDUAL ANALYSIS~%")
(format t "==================~%")
(dolist (item *analyzed-arithmetic*)
  (format t "~A: complexity=~A, non-zero-features=~A~%"
          (getf item :name) (getf item :complexity) (getf item :non-zero-features)))

;; Cosine similarity function
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

;; Compute similarities
(format t "~%SIMILARITY ANALYSIS~%")
(format t "===================~%")

(defparameter *arithmetic-similarities* nil)

(loop for i from 0 below (length *analyzed-arithmetic*)
      do (loop for j from (1+ i) below (length *analyzed-arithmetic*)
               do (let* ((item1 (nth i *analyzed-arithmetic*))
                         (item2 (nth j *analyzed-arithmetic*))
                         (sim (cosine-similarity (getf item1 :vector) (getf item2 :vector))))
                    (push (list (getf item1 :name) (getf item2 :name) sim) *arithmetic-similarities*))))

(setf *arithmetic-similarities* (sort *arithmetic-similarities* #'> :key #'third))

;; Show similarities
(format t "Similarities between arithmetic functions:~%")
(dolist (sim *arithmetic-similarities*)
  (when (> (third sim) 0.1)  ; Show only meaningful similarities
    (format t "  ~A ↔ ~A: ~,3F~%" (first sim) (second sim) (third sim))))

;; Pattern-based clustering
(format t "~%CLUSTERING BY FUNCTION PATTERNS~%")
(format t "===============================~%")

(defparameter *arithmetic-clusters* (make-hash-table :test 'equal))

(dolist (item *analyzed-arithmetic*)
  (let* ((name (getf item :name))
         (pattern (cond
                   ((search "ARITH" name) "arithmetic-operations")
                   ((search "FIND" name) "search-functions")
                   ((search "BUBBLE" name) "bubble-operations")
                   ((search "GATHER" name) "gather-operations")
                   ((search "HINT" name) "hint-functions")
                   ((search "DEFAULT" name) "default-functions")
                   (t "other"))))
    (push item (gethash pattern *arithmetic-clusters*))))

;; Display clusters
(maphash (lambda (pattern items)
           (when (> (length items) 0)
             (format t "~%Cluster: ~A (~A items)~%" pattern (length items))
             (format t "Members: ~{~A~^, ~}~%"
                     (mapcar (lambda (item) (getf item :name)) items))
             (let ((avg-complexity (/ (reduce #'+ items :key (lambda (x) (getf x :complexity)))
                                      (length items))))
               (format t "Average complexity: ~,1F~%" avg-complexity))))
         *arithmetic-clusters*)

(format t "~%ANALYSIS COMPLETE~%")
(format t "Arithmetic book clustering shows:~%")
(format t "1. Function groupings by naming patterns~%")
(format t "2. Feature-based similarity analysis~%")
(format t "3. Complexity distribution across arithmetic operations~%")

(sb-ext:exit)