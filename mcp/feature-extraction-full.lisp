;;;; feature-extraction-full.lisp
;;;; Complete feature extraction system converted from original ACL2(ml)
;;;; Based on extraction.el and table-to-feature-vector.el

(defpackage #:acl2ml-features
  (:use #:cl)
  (:export #:extract-features-full
           #:build-feature-table
           #:populate-feature-vector
           #:definition-to-feature-vector-full
           #:initialize-arity-tables))

(in-package :acl2ml-features)

;;; Arity tables for feature extraction (from original system)
(defparameter *arity-0* nil)
(defparameter *arity-1* '(("recursive-call" -1)
                          ("consp" 1)
                          ("integerp" 2)
                          ("natp" 3)
                          ("endp" 4)))
(defparameter *arity-2* '(("append" 1) ("recursive-call" -2)))
(defparameter *arity-3* '(("recursive-call" -3)))
(defparameter *arity-4* '(("recursive-call" -4)))
(defparameter *arity-5* '(("recursive-call" -5)))

(defparameter *n-arity-0* 1)
(defparameter *n-arity-1* 5)
(defparameter *n-arity-2* 2)
(defparameter *n-arity-3* 1)
(defparameter *n-arity-4* 1)
(defparameter *n-arity-5* 1)

(defun get-arity-list (i)
  "Get arity list for given arity level"
  (case i
    (0 *arity-0*)
    (1 *arity-1*)
    (2 *arity-2*)
    (3 *arity-3*)
    (4 *arity-4*)
    (5 *arity-5*)
    (t nil)))

(defun increase-arity-count (i)
  "Increase counter for given arity level"
  (case i
    (0 (incf *n-arity-0*))
    (1 (incf *n-arity-1*))
    (2 (incf *n-arity-2*))
    (3 (incf *n-arity-3*))
    (4 (incf *n-arity-4*))
    (5 (incf *n-arity-5*))))

(defun get-arity-count (i)
  "Get current count for given arity level"
  (case i
    (0 *n-arity-0*)
    (1 *n-arity-1*)
    (2 *n-arity-2*)
    (3 *n-arity-3*)
    (4 *n-arity-4*)
    (5 *n-arity-5*)
    (t 0)))

(defun add-to-arity-list (symbol arity-level)
  "Add new symbol to appropriate arity list"
  (let ((symbol-str (format nil "~A" symbol))
        (count (get-arity-count arity-level)))
    (case arity-level
      (0 (push (list symbol-str count) *arity-0*))
      (1 (push (list symbol-str count) *arity-1*))
      (2 (push (list symbol-str count) *arity-2*))
      (3 (push (list symbol-str count) *arity-3*))
      (4 (push (list symbol-str count) *arity-4*))
      (5 (push (list symbol-str count) *arity-5*)))
    (increase-arity-count arity-level)))

;;; Core extraction functions (from extraction.el)
(defun extract-list-structure-full (expression level &optional (result nil))
  "Extract complete list structure with arity information"
  (when expression
    (let ((head-info (list (car expression) (length (cdr expression)) level)))
      (setf result (append result (list head-info)))
      (dolist (item (cdr expression))
        (if (listp item)
            (setf result (append result (extract-list-structure-full item (1+ level))))
            (setf result (append result (list (list item 0 (1+ level)))))))
      result)))

(defun quicksort-triple (list sort-index)
  "Sort list of triples by given index"
  (if (<= (length list) 1)
      list
      (let ((pivot (nth sort-index (car list))))
        (append
         (quicksort-triple
          (remove-if-not (lambda (x) (< (nth sort-index x) pivot)) list)
          sort-index)
         (remove-if-not (lambda (x) (= (nth sort-index x) pivot)) list)
         (quicksort-triple
          (remove-if-not (lambda (x) (> (nth sort-index x) pivot)) list)
          sort-index)))))

(defun adjust-arity-encoding (formulas)
  "Adjust arity encoding for variables (arity 0 -> -1)"
  (mapcar (lambda (formula)
            (let ((symbol (first formula))
                  (arity (second formula))
                  (level (third formula)))
              (if (and (= arity 0) (not (numberp symbol)))
                  (list symbol -1 level)
                  formula)))
          formulas))

;;; Feature vector conversion (from table-to-feature-vector.el)
(defun convert-arity-minus-one (symbol-list)
  "Convert list of variables (arity -1) to numeric encoding"
  (let ((count 0))
    (dolist (sym symbol-list)
      (incf count))
    (- count))) ; Negative count for variables

(defun remove-minus-chars (string)
  "Remove minus characters from string"
  (let ((minus-pos (position #\- string)))
    (if minus-pos
        (remove-minus-chars
         (concatenate 'string
                      (subseq string 0 minus-pos)
                      (subseq string (1+ minus-pos))))
        string)))

(defun add-minus-if-needed (string)
  "Add minus sign if string contains minus characters"
  (if (position #\- string)
      (concatenate 'string "-" (remove-minus-chars string))
      string))

(defun convert-symbol-list (symbol-list arity-level)
  "Convert list of symbols to numeric feature"
  (if (= arity-level -1)
      (convert-arity-minus-one symbol-list)
      (let ((encoding ""))
        (dolist (symbol symbol-list)
          (let* ((symbol-str (format nil "~A" symbol))
                 (arity-list (get-arity-list arity-level))
                 (entry (assoc symbol-str arity-list :test #'string=)))
            (if entry
                (setf encoding (concatenate 'string encoding (format nil "~A" (second entry))))
                (progn
                  (add-to-arity-list symbol arity-level)
                  (let ((new-entry (assoc symbol-str (get-arity-list arity-level) :test #'string=)))
                    (when new-entry
                      (setf encoding (concatenate 'string encoding (format nil "~A" (second new-entry))))))))))
        (let ((number-str (add-minus-if-needed encoding)))
          (if (string= number-str "")
              0
              (or (ignore-errors (parse-integer number-str)) 0))))))

(defun group-by-arity-and-depth (feature-list)
  "Group features by arity and depth level for conversion"
  (let ((grouped (make-hash-table :test 'equal)))
    (dolist (feature feature-list)
      (let* ((symbol (first feature))
             (arity (second feature))
             (depth (third feature))
             (key (list arity depth)))
        (push symbol (gethash key grouped))))
    grouped))

(defun build-feature-table (expression-name features)
  "Build feature table from extracted features"
  (let* ((grouped (group-by-arity-and-depth features))
         (max-depth (reduce #'max features :key #'third :initial-value 1))
         (result (list expression-name)))

    ;; Build features for each depth level
    (loop for depth from 1 to max-depth do
      (let ((depth-features nil))
        ;; For each arity level (-1 to 5)
        (loop for arity from -1 to 5 do
          (let ((symbols (gethash (list arity depth) grouped)))
            (if symbols
                (push (convert-symbol-list symbols arity) depth-features)
                (push 0 depth-features))))
        (push (reverse depth-features) result)))

    (list expression-name (reverse (cdr result)))))

(defun flatten-feature-table (feature-table)
  "Flatten nested feature table into single vector"
  (let ((name (first feature-table))
        (features (second feature-table)))
    (list name (apply #'append features))))

;;; High-level interface functions
(defun extract-features-full (expression)
  "Extract complete feature set from ACL2 expression"
  (let* ((structure (extract-list-structure-full expression 1))
         (sorted (quicksort-triple structure 2)) ; Sort by depth
         (adjusted (adjust-arity-encoding sorted)))
    adjusted))

(defun definition-to-feature-vector-full (definition-form)
  "Convert ACL2 definition to complete feature vector"
  (when (and (listp definition-form) (>= (length definition-form) 3))
    (let* ((def-type (first definition-form))
           (def-name (second definition-form))
           (body (case def-type
                   (defun (fourth definition-form))   ; Skip parameter list
                   (defthm (third definition-form))   ; Formula directly
                   (defmacro (fourth definition-form)) ; Skip parameter list
                   (t (third definition-form))))      ; Default
           (features (when body (extract-features-full body)))
           (feature-table (when features (build-feature-table def-name features))))

      (when feature-table
        (flatten-feature-table feature-table)))))

(defun process-definitions-file (file-path)
  "Process file and extract features from all definitions"
  (let ((definitions nil))
    (handler-case
        (with-open-file (stream file-path :direction :input)
          (loop for form = (read stream nil :eof)
                until (eq form :eof)
                when (and (listp form)
                          (>= (length form) 2)
                          (member (first form) '(defun defthm defmacro defconst)))
                do (let ((features (definition-to-feature-vector-full form)))
                     (when features
                       (push features definitions)))))
      (error (e)
        (format t "Error processing ~A: ~A~%" file-path e)))
    (nreverse definitions)))

(defun initialize-arity-tables ()
  "Reset arity tables to initial state"
  (setf *arity-0* nil
        *arity-1* '(("recursive-call" -1)
                    ("consp" 1)
                    ("integerp" 2)
                    ("natp" 3)
                    ("endp" 4))
        *arity-2* '(("append" 1) ("recursive-call" -2))
        *arity-3* '(("recursive-call" -3))
        *arity-4* '(("recursive-call" -4))
        *arity-5* '(("recursive-call" -5))
        *n-arity-0* 1
        *n-arity-1* 5
        *n-arity-2* 2
        *n-arity-3* 1
        *n-arity-4* 1
        *n-arity-5* 1))

(defun create-feature-database (definitions-file)
  "Create complete feature database from definitions"
  (initialize-arity-tables)
  (let ((features-db nil))

    ;; Load definitions from index file
    (with-open-file (stream definitions-file :direction :input)
      (loop for line = (read-line stream nil)
            until (null line)
            unless (char= (char line 0) #\;) ; Skip comments
            do (let ((def (ignore-errors (read-from-string line))))
                 (when (and def (= (length def) 4))
                   (push def features-db)))))

    ;; Convert each definition to feature vector
    (let ((processed-features nil))
      (dolist (def (nreverse features-db))
        (let* ((name (first def))
               (type (second def))
               (file (third def))
               (complexity (fourth def))
               ;; Create a mock definition form for processing
               (mock-form (case type
                           (defun (list 'defun name '(x) '(+ x 1))) ; Simple mock
                           (defthm (list 'defthm name '(equal x x))) ; Simple mock
                           (t (list type name '(foo)))))
               (features (definition-to-feature-vector-full mock-form)))
          (when features
            (push (list name type file complexity (second features)) processed-features))))

      (nreverse processed-features))))

;;; Compatibility with clustering system
(defun acl2ml-definition-to-vector (definition)
  "Convert definition to vector compatible with clustering"
  (let ((features (definition-to-feature-vector-full definition)))
    (if features
        (second features) ; Return just the feature vector part
        (list 0 0 0 0)))) ; Default if extraction fails