;;;; extraction-bridge.lisp
;;;; ⭐ CORE ORCHESTRATION - Missing bridge from extraction-recursive.el
;;;;
;;;; This file implements the critical orchestration functions from
;;;; code/extraction-recursive.el that connect the verified components:
;;;;
;;;; PIPELINE: ACL2 content → extract-info → build-table → populate-table → flatten-table
;;;;
;;;; REPLACES: Buffer-based processing with direct content processing
;;;; CONNECTS: extraction.lisp + table-to-feature-vector.lisp → complete pipeline

(in-package :acl2ml-mcp)

;;; =======================================================================
;;; Global State Variables (from extraction-recursive.el)
;;; =======================================================================
;; These maintain processing state across the pipeline
;; CRITICAL: Must be managed carefully for session consistency

(defvar *definitions* nil
  "Global list of processed definitions: ((name . feature-data) ...)
   Corresponds to 'definitions' in extraction-recursive.el line 13")

(defvar *lemmas* nil
  "Global list of processed lemmas/theorems: ((name . feature-data) ...)
   Corresponds to 'lemmas' in extraction-recursive.el line 4")

(defvar *definitions-vectors* nil
  "Final feature vectors for definitions: ((name [numeric-vector]) ...)
   Corresponds to 'defs-vectors' in extraction-recursive.el")

(defvar *lemmas-vectors* nil
  "Final feature vectors for lemmas: ((name [numeric-vector]) ...)
   Corresponds to 'lemma-vectors' in extraction-recursive.el line 304")

;;; =======================================================================
;;; ACL2 Content Parsing (replaces buffer navigation)
;;; =======================================================================

(defun parse-acl2-expressions (acl2-string)
  "Parse ACL2 content string into list of S-expressions

   REPLACES: Buffer navigation from extract-tables-recursive() lines 87-94
   INPUT: String containing ACL2 code
   OUTPUT: List of parsed S-expressions"
  (let ((expressions '())
        (stream (make-string-input-stream acl2-string)))
    (handler-case
        (loop
          (let ((expr (read stream nil :eof)))
            (if (eq expr :eof)
                (return (nreverse expressions))
                (push expr expressions))))
      (error (e)
        (format t "Warning: Parse error in ACL2 content: ~A~%" e)
        (nreverse expressions)))))

(defun expression-type (expr)
  "Determine the type of ACL2 expression

   REPLACES: Logic from extract-tables-recursive() lines 95-129
   OUTPUT: :defthm, :defun, :include-book, :defmacro, or :other"
  (when (and (listp expr) (> (length expr) 0))
    (let ((first-symbol (car expr)))
      (when (symbolp first-symbol)
        (case (intern (string-upcase (symbol-name first-symbol)) :keyword)
          (:defthm :defthm)
          (:defun :defun)
          (:include-book :include-book)
          (:defmacro :defmacro)
          (otherwise :other))))))

;;; =======================================================================
;;; Main Processing Functions (from extraction-recursive.el)
;;; =======================================================================

(defun process-acl2-content (acl2-content content-type)
  "Main orchestration function - process ACL2 content into feature data

   REPLACES: extract-tables-recursive() from extraction-recursive.el lines 68-131
   FLOW: Parse content → Process each expression → Store in global variables

   INPUT: acl2-content (string), content-type ('definitions' or 'theorems')
   SIDE EFFECTS: Populates *definitions* and *lemmas* global variables"

  ;; Clear previous state (like setq lemmas/definitions nil in original)
  (setf *definitions* nil)
  (setf *lemmas* nil)

  ;; Parse ACL2 content into expressions
  (let ((expressions (parse-acl2-expressions acl2-content)))

    ;; Process each expression (replaces while loop from lines 87-129)
    (dolist (expr expressions)
      (let ((expr-type (expression-type expr)))

        (case expr-type
          ;; DEFTHM processing (lines 95-104)
          (:defthm
           (handler-case
               (let ((info (extract-info expr)))
                 (when info
                   (let ((lemma-name (car info)))
                     ;; Only add if not already present (like line 100 check)
                     (unless (find lemma-name *lemmas* :key #'car :test #'equal)
                       (push info *lemmas*)))))
             (error (e)
               (format t "Warning: Failed to process theorem ~A: ~A~%"
                      (if (> (length expr) 1) (cadr expr) "UNKNOWN") e))))

          ;; DEFUN processing (lines 106-124)
          (:defun
           (handler-case
               (let* ((has-declare (and (> (length expr) 3)
                                      (listp (nth 3 expr))
                                      (eq (car (nth 3 expr)) 'declare)))
                      ;; Remove declare clause for processing (lines 107-110)
                      (clean-expr (if has-declare
                                    (list (nth 0 expr) (nth 1 expr) (nth 2 expr) (nth 4 expr))
                                    (list (nth 0 expr) (nth 1 expr) (nth 2 expr) (nth 3 expr))))
                      (info (extract-info clean-expr)))
                 (when info
                   ;; Add recursive call detection (line 109)
                   (setf info (search-for-recursive-calls info))
                   (let ((def-name (car info)))
                     ;; Only add if not already present (like line 113 check)
                     (unless (find def-name *definitions* :key #'car :test #'equal)
                       (push info *definitions*)))))
             (error (e)
               (format t "Warning: Failed to process definition ~A: ~A~%"
                      (if (> (length expr) 1) (cadr expr) "UNKNOWN") e))))

          ;; DEFMACRO processing (lines 193-204)
          (:defmacro
           (handler-case
               (let* ((name (cadr expr))
                      (params (caddr expr))
                      ;; Create simplified expansion for feature extraction
                      (simple-expr (list 'defmacro name params)))
                 (let ((info (extract-info simple-expr)))
                   (when info
                     (let ((macro-name (car info)))
                       (unless (find macro-name *definitions* :key #'car :test #'equal)
                         (push info *definitions*))))))
             (error (e)
               (format t "Warning: Failed to process macro ~A: ~A~%"
                      (if (> (length expr) 1) (cadr expr) "UNKNOWN") e))))

          ;; Skip include-book and other expressions
          (otherwise nil))))

    ;; Return summary
    (list :definitions-count (length *definitions*)
          :lemmas-count (length *lemmas*)
          :processed-expressions (length expressions))))

(defun convert-to-feature-vectors (content-type)
  "Convert processed expressions to final feature vectors

   REPLACES: convert-recursive() and convert-recursive-several-libraries-defs()
   From extraction-recursive.el lines 306-328

   INPUT: content-type ('definitions' or 'theorems')
   OUTPUT: List of (name [feature-vector]) pairs
   SIDE EFFECTS: Populates *definitions-vectors* or *lemmas-vectors*"

  (case (intern (string-upcase content-type) :keyword)

    ;; Convert definitions (lines 324-328)
    (:definitions
     (setf *definitions-vectors* nil)
     (dolist (def-info *definitions*)
       (handler-case
           (let* ((built-table (build-table def-info))
                  (populated-table (populate-table built-table))
                  (final-vector (flatten-table populated-table)))
             (push final-vector *definitions-vectors*))
         (error (e)
           (format t "Warning: Failed to vectorize definition ~A: ~A~%"
                  (car def-info) e))))
     (nreverse *definitions-vectors*))

    ;; Convert lemmas/theorems (lines 306-309)
    (:theorems
     (setf *lemmas-vectors* nil)
     (dolist (lemma-info *lemmas*)
       (handler-case
           (let* ((built-table (build-table lemma-info))
                  (populated-table (populate-table built-table))
                  (final-vector (flatten-table populated-table)))
             (push final-vector *lemmas-vectors*))
         (error (e)
           (format t "Warning: Failed to vectorize theorem ~A: ~A~%"
                  (car lemma-info) e))))
     (nreverse *lemmas-vectors*))

    (otherwise
     (error "Unknown content-type: ~A. Must be 'definitions' or 'theorems'" content-type))))

;;; =======================================================================
;;; Utility Functions
;;; =======================================================================

(defun get-definitions-vectors ()
  "Get current definitions feature vectors"
  *definitions-vectors*)

(defun get-lemmas-vectors ()
  "Get current lemmas feature vectors"
  *lemmas-vectors*)

(defun clear-processing-state ()
  "Clear all global processing state"
  (setf *definitions* nil
        *lemmas* nil
        *definitions-vectors* nil
        *lemmas-vectors* nil))

(defun processing-summary ()
  "Get summary of current processing state"
  (list :definitions (length *definitions*)
        :lemmas (length *lemmas*)
        :definition-vectors (length *definitions-vectors*)
        :lemma-vectors (length *lemmas-vectors*)))

;;; =======================================================================
;;; Complete Pipeline Function (for MCP integration)
;;; =======================================================================

(defun complete-extraction-pipeline (acl2-content content-type)
  "Complete pipeline: ACL2 content → feature vectors

   This is the main entry point for MCP tools

   INPUT: acl2-content (string), content-type ('definitions' or 'theorems')
   OUTPUT: List of (name [feature-vector]) pairs"

  ;; Step 1: Process content into intermediate representations
  (let ((process-result (process-acl2-content acl2-content content-type)))

    ;; Step 2: Convert to final feature vectors
    (let ((vectors (convert-to-feature-vectors content-type)))

      ;; Return results with metadata
      (list :vectors vectors
            :processing-summary process-result
            :final-count (length vectors)))))