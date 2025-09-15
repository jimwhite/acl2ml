;;;; mcp-tools.lisp
;;;; MCP tool definitions for ACL2(ml) functionality

(in-package #:acl2ml-mcp)

;;; Initialize API
(openrpc-server:define-api (acl2ml-tools :title "ACL2(ml) Machine Learning Assistant")
  (:summary "Provides AI-powered assistance for ACL2 theorem proving using machine learning")
  (:version "2.0.0"))

;;; Tool: Regenerate Definitions Index
(40ants-mcp/tools:define-tool (acl2ml-tools regenerate-definitions-index)
    (&key (scan-all nil) (books nil))
  (:summary "Regenerate the definitions index for ACL2 8.6 books")
  (:param scan-all boolean "If true, scan all books in the ACL2 books directory")
  (:param books (soft-list-of string) "List of specific books to index (e.g., 'arithmetic/top')")
  (:result (soft-list-of text-content))

  (handler-case
      (let ((books-list (when books (mapcar #'string books))))
        (regenerate-definitions-index
         :books-list books-list
         :scan-all scan-all)

        (let ((total-definitions (hash-table-count *definitions-index*)))
          (list (make-instance 'text-content
                               :text (format nil "Successfully regenerated definitions index with ~A definitions.~%~
                                                 Indexed books: ~{~A~^, ~}~%~
                                                 Use 'list-definitions' to explore the index."
                                             total-definitions
                                             (or books-list '("default system books")))))))
    (error (e)
      (list (make-instance 'text-content
                           :text (format nil "Error regenerating definitions index: ~A" e))))))

;;; Tool: List Definitions
(40ants-mcp/tools:define-tool (acl2ml-tools list-definitions)
    (&key (book-filter nil) (type-filter nil) (limit 20))
  (:summary "List ACL2 definitions from the index")
  (:param book-filter string "Filter by book name (optional)")
  (:param type-filter string "Filter by definition type: defun, defthm, defmacro, etc. (optional)")
  (:param limit integer "Maximum number of results to return (default: 20)")
  (:result (soft-list-of text-content))

  (handler-case
      (let* ((type-sym (when type-filter (intern (string-upcase type-filter) :keyword)))
             (definitions (list-definitions
                          :book-filter book-filter
                          :type-filter type-sym))
             (limited-defs (subseq definitions 0 (min limit (length definitions)))))

        (if definitions
            (list (make-instance 'text-content
                                 :text (format nil "Found ~A definitions~@[ in book '~A'~]~@[ of type '~A'~]:~%~%~{~A~%~}"
                                             (length definitions)
                                             book-filter
                                             type-filter
                                             (mapcar (lambda (def)
                                                      (format nil "~A (~A) - ~A [~A]"
                                                              (acl2-definition-name def)
                                                              (acl2-definition-type def)
                                                              (acl2-definition-book def)
                                                              (acl2-definition-arity def)))
                                                    limited-defs))))
            (list (make-instance 'text-content
                                 :text "No definitions found matching the criteria."))))
    (error (e)
      (list (make-instance 'text-content
                           :text (format nil "Error listing definitions: ~A" e))))))

;;; Tool: Analyze Theorem Structure
(40ants-mcp/tools:define-tool (acl2ml-tools analyze-theorem)
    (theorem-expression)
  (:summary "Analyze the logical structure and complexity of an ACL2 theorem")
  (:param theorem-expression string "The theorem expression to analyze (S-expression format)")
  (:result (soft-list-of text-content))

  (handler-case
      (let* ((expr (read-from-string theorem-expression))
             (analysis (analyze-theorem-structure expr)))

        (if analysis
            (list (make-instance 'text-content
                                 :text (format nil "Theorem Analysis:~%~
                                               Name: ~A~%~
                                               Logical Structure: ~A~%~
                                               Complexity Score: ~A~%~
                                               Dependencies: ~{~A~^, ~}~%~
                                               Hypothesis: ~A~%~
                                               Conclusion: ~A"
                                             (or (theorem-analysis-name analysis) "anonymous")
                                             (theorem-analysis-logical-structure analysis)
                                             (theorem-analysis-complexity-score analysis)
                                             (theorem-analysis-dependencies analysis)
                                             (theorem-analysis-hypothesis analysis)
                                             (theorem-analysis-conclusion analysis))))
            (list (make-instance 'text-content
                                 :text "Could not analyze theorem structure. Please check the expression format."))))
    (error (e)
      (list (make-instance 'text-content
                           :text (format nil "Error analyzing theorem: ~A" e))))))

;;; Tool: Extract Features
(40ants-mcp/tools:define-tool (acl2ml-tools extract-features)
    (expression)
  (:summary "Extract machine learning features from an ACL2 expression")
  (:param expression string "The ACL2 expression to analyze (S-expression format)")
  (:result (soft-list-of text-content))

  (handler-case
      (let* ((expr (read-from-string expression))
             (features (extract-features expr))
             (flat-features (flatten-feature-vector features)))

        (list (make-instance 'text-content
                             :text (format nil "Feature Extraction Results:~%~
                                           Expression: ~A~%~%~
                                           Structural Features:~%~
                                           ~{  ~A: ~A~%~}~%~
                                           Complexity Features:~%~
                                           ~{  ~A: ~A~%~}~%~
                                           Symbol Features (top 10):~%~
                                           ~{  ~A (arity: ~A, depth: ~A)~%~}~%~
                                           Feature Vector: [~{~A~^, ~}]"
                                           expr
                                           (feature-vector-structure features)
                                           (feature-vector-complexity features)
                                           (mapcar (lambda (s) (format nil "~A ~A ~A" (first s) (second s) (third s)))
                                                  (subseq (feature-vector-symbols features)
                                                         0 (min 10 (length (feature-vector-symbols features)))))
                                           flat-features))))
    (error (e)
      (list (make-instance 'text-content
                           :text (format nil "Error extracting features: ~A" e))))))

;;; Tool: Find Similar Lemmas
(40ants-mcp/tools:define-tool (acl2ml-tools find-similar-lemmas)
    (theorem-expression &key (similarity-threshold 0.7) (max-results 5))
  (:summary "Find lemmas similar to a given theorem using machine learning similarity")
  (:param theorem-expression string "The theorem to find similarities for")
  (:param similarity-threshold number "Minimum similarity score (0.0-1.0, default: 0.7)")
  (:param max-results integer "Maximum number of similar lemmas to return (default: 5)")
  (:result (soft-list-of text-content))

  (handler-case
      (let* ((expr (read-from-string theorem-expression))
             (similar-lemmas (suggest-similar-lemmas expr
                                                    :similarity-threshold similarity-threshold
                                                    :max-results max-results)))

        (if similar-lemmas
            (list (make-instance 'text-content
                                 :text (format nil "Found ~A similar lemmas:~%~%~{~A~%~}"
                                             (length similar-lemmas)
                                             (mapcar (lambda (lemma)
                                                      (format nil "• ~A (~A) [~A]~%  Book: ~A~%  Body: ~A~%"
                                                              (acl2-definition-name lemma)
                                                              (acl2-definition-type lemma)
                                                              (acl2-definition-arity lemma)
                                                              (acl2-definition-book lemma)
                                                              (acl2-definition-body lemma)))
                                                    similar-lemmas))))
            (list (make-instance 'text-content
                                 :text (format nil "No similar lemmas found with similarity >= ~A" similarity-threshold)))))
    (error (e)
      (list (make-instance 'text-content
                           :text (format nil "Error finding similar lemmas: ~A" e))))))

;;; Tool: Recommend Lemmas for Proof
(40ants-mcp/tools:define-tool (acl2ml-tools recommend-lemmas)
    (goal-theorem &key (max-recommendations 5))
  (:summary "Recommend lemmas that might be useful for proving a theorem")
  (:param goal-theorem string "The theorem goal to find helpful lemmas for")
  (:param max-recommendations integer "Maximum number of recommendations (default: 5)")
  (:result (soft-list-of text-content))

  (handler-case
      (let* ((expr (read-from-string goal-theorem))
             (recommendations (recommend-lemmas-for-proof expr
                                                         :max-recommendations max-recommendations)))

        (if recommendations
            (list (make-instance 'text-content
                                 :text (format nil "Lemma Recommendations for Proof:~%~%~{~A~%~}"
                                             (mapcar (lambda (rec)
                                                      (format nil "• ~A (score: ~,2F)~%  Reason: ~A~%  Book: ~A~%  Proof strategy: ~{~A~^, ~}~%"
                                                              (acl2-definition-name (lemma-recommendation-lemma-def rec))
                                                              (lemma-recommendation-similarity-score rec)
                                                              (lemma-recommendation-recommendation-reason rec)
                                                              (acl2-definition-book (lemma-recommendation-lemma-def rec))
                                                              (lemma-recommendation-proof-strategy-match rec)))
                                                    recommendations))))
            (list (make-instance 'text-content
                                 :text "No lemma recommendations found for this theorem."))))
    (error (e)
      (list (make-instance 'text-content
                           :text (format nil "Error generating recommendations: ~A" e))))))

;;; Tool: Search Definitions
(40ants-mcp/tools:define-tool (acl2ml-tools search-definitions)
    (pattern &key (field "name") (limit 20))
  (:summary "Search definitions by pattern in specified field")
  (:param pattern string "Search pattern to match")
  (:param field string "Field to search in: name, book, or type (default: name)")
  (:param limit integer "Maximum results to return (default: 20)")
  (:result (soft-list-of text-content))

  (handler-case
      (let* ((field-key (intern (string-upcase field) :keyword))
             (results (search-definitions pattern :field field-key))
             (limited-results (subseq results 0 (min limit (length results)))))

        (if results
            (list (make-instance 'text-content
                                 :text (format nil "Found ~A definitions matching '~A' in ~A:~%~%~{~A~%~}"
                                             (length results)
                                             pattern
                                             field
                                             (mapcar (lambda (def)
                                                      (format nil "~A (~A) - ~A"
                                                              (acl2-definition-name def)
                                                              (acl2-definition-type def)
                                                              (acl2-definition-book def)))
                                                    limited-results))))
            (list (make-instance 'text-content
                                 :text (format nil "No definitions found matching '~A' in ~A" pattern field)))))
    (error (e)
      (list (make-instance 'text-content
                           :text (format nil "Error searching definitions: ~A" e))))))

;;; Tool: Export Features to CSV
(40ants-mcp/tools:define-tool (acl2ml-tools export-features-csv)
    (&key (filename "acl2ml-features.csv"))
  (:summary "Export all definition features to CSV file for machine learning")
  (:param filename string "Output CSV filename (default: acl2ml-features.csv)")
  (:result (soft-list-of text-content))

  (handler-case
      (progn
        ;; First extract features from all definitions if not already done
        (let ((processed-count (extract-all-definition-features)))
          (export-features-to-csv filename)
          (list (make-instance 'text-content
                               :text (format nil "Successfully exported features for ~A definitions to ~A.~%~
                                             This CSV file can be used for machine learning model training."
                                           processed-count filename)))))
    (error (e)
      (list (make-instance 'text-content
                           :text (format nil "Error exporting features: ~A" e))))))

;;; Tool: Get Definition Details
(40ants-mcp/tools:define-tool (acl2ml-tools get-definition)
    (definition-name)
  (:summary "Get detailed information about a specific definition")
  (:param definition-name string "Name of the definition to retrieve")
  (:result (soft-list-of text-content))

  (handler-case
      (let* ((name-sym (intern (string-upcase definition-name)))
             (definition (find-definition name-sym)))

        (if definition
            (list (make-instance 'text-content
                                 :text (format nil "Definition Details:~%~
                                               Name: ~A~%~
                                               Type: ~A~%~
                                               Arity: ~A~%~
                                               Book: ~A~%~
                                               Body: ~A~%~
                                               Dependencies: ~{~A~^, ~}~%~
                                               Guards: ~A"
                                             (acl2-definition-name definition)
                                             (acl2-definition-type definition)
                                             (acl2-definition-arity definition)
                                             (acl2-definition-book definition)
                                             (acl2-definition-body definition)
                                             (acl2-definition-dependencies definition)
                                             (acl2-definition-guards definition))))
            (list (make-instance 'text-content
                                 :text (format nil "Definition '~A' not found in index. ~
                                               Try regenerating the index or check the name spelling."
                                             definition-name)))))
    (error (e)
      (list (make-instance 'text-content
                           :text (format nil "Error retrieving definition: ~A" e))))))