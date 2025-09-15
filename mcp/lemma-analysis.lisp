;;;; lemma-analysis.lisp
;;;; Lemma analysis and theorem structure analysis for ACL2(ml)

(in-package #:acl2ml-mcp)

;;; Theorem structure analysis
(defstruct theorem-analysis
  name
  hypothesis           ; List of hypotheses (antecedent)
  conclusion          ; Conclusion (consequent)
  logical-structure   ; implies, iff, equal, etc.
  complexity-score    ; Computed complexity
  proof-hints        ; Associated proof hints
  dependencies       ; Referenced definitions/lemmas
  )

(defun analyze-theorem-structure (theorem-form)
  "Analyze the logical structure of a theorem"
  (when (and (listp theorem-form) (>= (length theorem-form) 2))
    (let* ((name (when (symbolp (first theorem-form)) (first theorem-form)))
           (formula (if name (second theorem-form) (first theorem-form)))
           (hints (when name (nthcdr 2 theorem-form))))

      (multiple-value-bind (hypothesis conclusion structure)
          (parse-logical-structure formula)

        (make-theorem-analysis
         :name name
         :hypothesis hypothesis
         :conclusion conclusion
         :logical-structure structure
         :complexity-score (compute-theorem-complexity formula)
         :proof-hints hints
         :dependencies (extract-theorem-dependencies formula))))))

(defun parse-logical-structure (formula)
  "Parse logical structure of a formula, returning hypothesis, conclusion, and structure type"
  (cond
    ;; (implies P Q) form
    ((and (listp formula) (eq (first formula) 'implies) (= (length formula) 3))
     (values (second formula) (third formula) 'implies))

    ;; (iff P Q) form
    ((and (listp formula) (eq (first formula) 'iff) (= (length formula) 3))
     (values (second formula) (third formula) 'iff))

    ;; (equal P Q) form
    ((and (listp formula) (eq (first formula) 'equal) (= (length formula) 3))
     (values nil formula 'equal))

    ;; Direct formula (no implication)
    (t
     (values nil formula 'direct))))

(defun compute-theorem-complexity (formula)
  "Compute complexity score for a theorem"
  (let ((depth 0)
        (logical-connectives 0)
        (quantifiers 0)
        (function-calls 0))

    (labels ((analyze-node (node current-depth)
               (setf depth (max depth current-depth))

               (if (atom node)
                   (when (symbolp node)
                     (incf function-calls))
                   (progn
                     (let ((head (first node)))
                       (case head
                         ((and or not) (incf logical-connectives))
                         ((forall exists) (incf quantifiers))
                         (t (when (symbolp head) (incf function-calls)))))
                     (dolist (child (cdr node))
                       (analyze-node child (1+ current-depth)))))))

      (analyze-node formula 0)

      ;; Weighted complexity score
      (+ (* depth 2)
         (* logical-connectives 3)
         (* quantifiers 5)
         function-calls))))

(defun extract-theorem-dependencies (formula)
  "Extract function and predicate dependencies from theorem"
  (let ((deps (make-hash-table :test 'eq)))
    (labels ((collect-symbols (node)
               (if (atom node)
                   (when (and (symbolp node) (not (keywordp node)))
                     (setf (gethash node deps) t))
                   (dolist (child node)
                     (collect-symbols child)))))
      (collect-symbols formula)
      (loop for sym being the hash-keys of deps collect sym))))

;;; Similarity analysis for lemmas
(defun suggest-similar-lemmas (target-theorem &key (similarity-threshold 0.7) (max-results 10))
  "Suggest lemmas similar to the target theorem"
  (let ((target-analysis (analyze-theorem-structure target-theorem))
        (candidates nil))

    ;; Find theorem definitions
    (maphash (lambda (name definition)
               (declare (ignore name))
               (when (eq (acl2-definition-type definition) 'defthm)
                 (let* ((def-analysis (analyze-theorem-structure (acl2-definition-body definition)))
                        (similarity (compute-theorem-similarity target-analysis def-analysis)))
                   (when (>= similarity similarity-threshold)
                     (push (list definition similarity def-analysis) candidates)))))
             *definitions-index*)

    ;; Sort by similarity and return top results
    (mapcar #'first
            (subseq (sort candidates (lambda (a b) (> (second a) (second b))))
                    0 (min max-results (length candidates))))))

(defun compute-theorem-similarity (analysis1 analysis2)
  "Compute similarity between two theorem analyses"
  (when (and analysis1 analysis2)
    (let ((structure-score (if (eq (theorem-analysis-logical-structure analysis1)
                                  (theorem-analysis-logical-structure analysis2))
                              1.0 0.5))
          (complexity-score (let ((c1 (theorem-analysis-complexity-score analysis1))
                                 (c2 (theorem-analysis-complexity-score analysis2)))
                             (if (= (max c1 c2) 0)
                                 1.0
                                 (- 1.0 (/ (abs (- c1 c2)) (max c1 c2))))))
          (dependency-score (compute-dependency-similarity
                            (theorem-analysis-dependencies analysis1)
                            (theorem-analysis-dependencies analysis2))))

      ;; Weighted average
      (/ (+ (* structure-score 0.3)
            (* complexity-score 0.2)
            (* dependency-score 0.5))
         1.0))))

(defun compute-dependency-similarity (deps1 deps2)
  "Compute Jaccard similarity between dependency sets"
  (let ((set1 (make-hash-table :test 'eq))
        (set2 (make-hash-table :test 'eq))
        (intersection-size 0)
        (union-size 0))

    ;; Build hash sets
    (dolist (dep deps1) (setf (gethash dep set1) t))
    (dolist (dep deps2) (setf (gethash dep set2) t))

    ;; Compute intersection
    (maphash (lambda (dep present)
               (declare (ignore present))
               (when (gethash dep set2)
                 (incf intersection-size)))
             set1)

    ;; Compute union size
    (maphash (lambda (dep present) (declare (ignore dep present)) (incf union-size)) set1)
    (maphash (lambda (dep present)
               (declare (ignore present))
               (unless (gethash dep set1)
                 (incf union-size)))
             set2)

    (if (> union-size 0)
        (/ intersection-size union-size)
        (if (and (zerop (length deps1)) (zerop (length deps2))) 1.0 0.0))))

;;; Proof strategy analysis
(defun analyze-proof-strategy (theorem-def)
  "Analyze the proof strategy used in a theorem"
  (let ((hints (when (acl2-definition-body theorem-def)
                 (nthcdr 2 (acl2-definition-body theorem-def))))
        (strategies nil))

    (when hints
      (dolist (hint hints)
        (when (listp hint)
          (cond
            ((member :induct hint) (push 'induction strategies))
            ((member :use hint) (push 'use-lemma strategies))
            ((member :cases hint) (push 'case-analysis strategies))
            ((member :expand hint) (push 'expansion strategies))
            ((member :in-theory hint) (push 'theory-control strategies))))))

    strategies))

;;; Lemma recommendation system
(defstruct lemma-recommendation
  lemma-def
  similarity-score
  recommendation-reason
  proof-strategy-match
  )

(defun recommend-lemmas-for-proof (goal-theorem &key (max-recommendations 5))
  "Recommend lemmas that might be useful for proving a theorem"
  (let ((goal-analysis (analyze-theorem-structure goal-theorem))
        (recommendations nil))

    ;; Find potentially useful lemmas
    (maphash (lambda (name definition)
               (declare (ignore name))
               (when (eq (acl2-definition-type definition) 'defthm)
                 (let* ((lemma-analysis (analyze-theorem-structure (acl2-definition-body definition)))
                        (similarity (compute-theorem-similarity goal-analysis lemma-analysis))
                        (dependency-overlap (intersection
                                           (theorem-analysis-dependencies goal-analysis)
                                           (theorem-analysis-dependencies lemma-analysis)))
                        (reason nil))

                   ;; Determine recommendation reason
                   (cond
                     ((> similarity 0.8) (setf reason "High structural similarity"))
                     ((> (length dependency-overlap) 2) (setf reason "Shared dependencies"))
                     ((and (eq (theorem-analysis-logical-structure lemma-analysis) 'implies)
                           (member (theorem-analysis-conclusion lemma-analysis)
                                  (flatten-formula (theorem-analysis-hypothesis goal-analysis))))
                      (setf reason "Conclusion matches goal hypothesis"))
                     ((> similarity 0.5) (setf reason "Moderate structural similarity")))

                   (when reason
                     (push (make-lemma-recommendation
                            :lemma-def definition
                            :similarity-score similarity
                            :recommendation-reason reason
                            :proof-strategy-match (analyze-proof-strategy definition))
                           recommendations)))))
             *definitions-index*)

    ;; Sort and return top recommendations
    (subseq (sort recommendations
                  (lambda (a b) (> (lemma-recommendation-similarity-score a)
                                  (lemma-recommendation-similarity-score b))))
            0 (min max-recommendations (length recommendations)))))

(defun flatten-formula (formula)
  "Flatten a formula to extract all sub-formulas"
  (if (atom formula)
      (list formula)
      (cons formula
            (apply #'append (mapcar #'flatten-formula (cdr formula))))))

;;; Proof obligation analysis
(defun extract-proof-obligations (theorem-form)
  "Extract subgoals and proof obligations from a theorem"
  (let ((analysis (analyze-theorem-structure theorem-form))
        (obligations nil))

    (when analysis
      ;; Extract obligations from hypothesis
      (when (theorem-analysis-hypothesis analysis)
        (let ((hyp-obligations (extract-obligations-from-formula
                               (theorem-analysis-hypothesis analysis))))
          (setf obligations (append obligations hyp-obligations))))

      ;; Extract obligations from conclusion
      (when (theorem-analysis-conclusion analysis)
        (let ((concl-obligations (extract-obligations-from-formula
                                 (theorem-analysis-conclusion analysis))))
          (setf obligations (append obligations concl-obligations)))))

    obligations))

(defun extract-obligations-from-formula (formula)
  "Extract proof obligations from a logical formula"
  (cond
    ((atom formula) nil)

    ;; Conjunction: all conjuncts are obligations
    ((eq (first formula) 'and)
     (apply #'append (mapcar #'extract-obligations-from-formula (cdr formula))))

    ;; Implication: antecedent creates obligations
    ((eq (first formula) 'implies)
     (extract-obligations-from-formula (second formula)))

    ;; Function application: may require termination/guard proofs
    ((symbolp (first formula))
     (list formula))

    (t nil)))