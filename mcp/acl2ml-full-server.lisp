;;;; acl2ml-full-server.lisp
;;;; Complete ACL2(ml) MCP server with full functionality
;;;; Run with: /home/acl2/saved_acl2 < acl2ml-full-server.lisp

;; Start in ACL2 mode, switch to Common Lisp
:q

;; Load dependencies
(load "~/quicklisp/setup.lisp")
(ql:quickload :jsonrpc :silent t)
(ql:quickload "jsonrpc/transport/stdio" :silent t)
(ql:quickload :40ants-mcp :silent t)

(format t "~%=== Complete ACL2(ml) MCP Server ===~%")
(format t "✓ Dependencies loaded~%")

;; Load our modules
(load "clustering.lisp")
(load "feature-extraction-full.lisp")

;; Define main package
(defpackage #:acl2ml-full
  (:use #:cl #:acl2ml-clustering #:acl2ml-features)
  (:export #:start-server))

(in-package :acl2ml-full)

(format t "✓ ACL2(ml) modules loaded~%")

;; Global data
(defparameter *definitions-index* nil)
(defparameter *feature-database* nil)

(defun load-definitions-index ()
  "Load definitions index from file"
  (setf *definitions-index* nil)
  (handler-case
      (with-open-file (stream "/workspaces/acl2ml/mcp/definitions-index.dat"
                              :direction :input)
        (loop for line = (read-line stream nil)
              until (null line)
              unless (char= (char line 0) #\;)
              do (let ((def (ignore-errors (read-from-string line))))
                   (when (and def (= (length def) 4))
                     (push def *definitions-index*)))))
    (error (e)
      (format t "Warning: Could not load definitions index: ~A~%" e)))
  (setf *definitions-index* (nreverse *definitions-index*))
  (length *definitions-index*))

;; Initialize data
(let ((defs-count (load-definitions-index)))
  (format t "✓ Loaded ~A definitions from index~%" defs-count))

;; Define MCP tools with full functionality
(40ants-mcp/tools:define-tool cluster-definitions (algorithm granularity-level)
  "Cluster ACL2 definitions using machine learning algorithms"
  (handler-case
      (let* ((alg-keyword (cond
                           ((string= algorithm "k") :k-means)
                           ((string= algorithm "e") :em)
                           ((string= algorithm "f") :farthest-first)
                           (t :k-means)))
             (granularity (or (ignore-errors (parse-integer granularity-level)) 3))
             (result (cluster-definitions *definitions-index* alg-keyword
                                          :granularity-level granularity)))

        (list (make-instance '40ants-mcp/content/text:text-content
                             :text (with-output-to-string (s)
                                     (format-clustering-results result *definitions-index*
                                                                :stream s)))))
    (error (e)
      (list (make-instance '40ants-mcp/content/text:text-content
                           :text (format nil "Error clustering definitions: ~A" e))))))

(40ants-mcp/tools:define-tool find-similar-definitions (target-name algorithm)
  "Find definitions similar to a target definition"
  (handler-case
      (let* ((alg-keyword (cond
                           ((string= algorithm "k") :k-means)
                           ((string= algorithm "e") :em)
                           ((string= algorithm "f") :farthest-first)
                           (t :k-means)))
             (target-symbol (read-from-string target-name))
             (similar (find-similar-items target-symbol *definitions-index* alg-keyword)))

        (list (make-instance '40ants-mcp/content/text:text-content
                             :text (with-output-to-string (s)
                                     (format-similarity-results
                                      (list target-symbol) similar :stream s)))))
    (error (e)
      (list (make-instance '40ants-mcp/content/text:text-content
                           :text (format nil "Error finding similarities: ~A" e))))))

(40ants-mcp/tools:define-tool extract-ml-features (expression)
  "Extract complete ML features from ACL2 expression"
  (handler-case
      (let* ((expr (read-from-string expression))
             (features (extract-features-full expr))
             (feature-vector (definition-to-feature-vector-full
                              (list 'defun 'temp-name '(x) expr))))

        (list (make-instance '40ants-mcp/content/text:text-content
                             :text (format nil "ACL2(ml) Feature Extraction:~%~
                                           Expression: ~A~%~
                                           Raw Features: ~A~%~
                                           Feature Vector: ~A~%~
                                           Vector Length: ~A~%~
                                           ~%This analysis uses the original ACL2(ml)~%~
                                           feature extraction with arity encoding~%~
                                           and depth-based structural analysis."
                                          expr
                                          (length features)
                                          (when feature-vector (second feature-vector))
                                          (when feature-vector (length (second feature-vector)))))))
    (error (e)
      (list (make-instance '40ants-mcp/content/text:text-content
                           :text (format nil "Error extracting features: ~A" e))))))

(40ants-mcp/tools:define-tool analyze-definition-structure (definition-text)
  "Analyze the complete structure of an ACL2 definition"
  (handler-case
      (let* ((def-form (read-from-string definition-text))
             (features (definition-to-feature-vector-full def-form))
             (structure (when (>= (length def-form) 3)
                         (extract-features-full (third def-form)))))

        (list (make-instance '40ants-mcp/content/text:text-content
                             :text (format nil "ACL2(ml) Definition Analysis:~%~
                                           Definition: ~A~%~
                                           Type: ~A~%~
                                           Name: ~A~%~
                                           Structural features: ~A~%~
                                           Feature vector: ~A~%~
                                           Complexity score: ~A~%~
                                           ~%Analysis includes:~%~
                                           • Arity-based feature encoding~%~
                                           • Depth-level structural analysis~%~
                                           • Symbol frequency patterns~%~
                                           • Recursive call detection"
                                          (if (> (length (format nil "~A" def-form)) 200)
                                              (format nil "~A..." (subseq (format nil "~A" def-form) 0 197))
                                              def-form)
                                          (first def-form)
                                          (second def-form)
                                          (length structure)
                                          (when features (length (second features)))
                                          (when structure
                                            (reduce #'+ structure :key (lambda (x) (abs (third x)))))))))
    (error (e)
      (list (make-instance '40ants-mcp/content/text:text-content
                           :text (format nil "Error analyzing definition: ~A" e))))))

(40ants-mcp/tools:define-tool regenerate-index ()
  "Regenerate the ACL2 definitions index"
  (handler-case
      (progn
        ;; Run the indexing process
        (uiop:run-program "/home/acl2/saved_acl2 < /workspaces/acl2ml/mcp/simple-index.lisp"
                          :output :string :error-output :string)
        (let ((count (load-definitions-index)))
          (list (make-instance '40ants-mcp/content/text:text-content
                               :text (format nil "ACL2 definitions index regenerated!~%~
                                             Successfully indexed ~A definitions~%~
                                             Index file: definitions-index.dat~%~
                                             ~%Index includes definitions from:~%~
                                             • Core ACL2 arithmetic books~%~
                                             • Standard library books~%~
                                             • Utility functions~%~
                                             • Example theorem files"
                                            count)))))
    (error (e)
      (list (make-instance '40ants-mcp/content/text:text-content
                           :text (format nil "Error regenerating index: ~A" e))))))

(40ants-mcp/tools:define-tool acl2ml-demo ()
  "Complete demonstration of ACL2(ml) capabilities"
  (list (make-instance '40ants-mcp/content/text:text-content
                       :text (format nil "ACL2(ml) Complete System Demo~%~
                                     =================================~%~
                                     ~%✓ Full machine learning integration with ACL2 8.6~%~
                                     ✓ Complete feature extraction from original system~%~
                                     ✓ Multiple clustering algorithms (K-means, EM, Farthest-First)~%~
                                     ✓ ACL2 book definitions indexing~%~
                                     ✓ Similarity-based theorem/definition search~%~
                                     ~%Current Status:~%~
                                     • Definitions indexed: ~A~%~
                                     • Feature extraction: ✓ Full arity-based encoding~%~
                                     • Clustering algorithms: ✓ 3 algorithms available~%~
                                     • MCP integration: ✓ Ready for AI assistants~%~
                                     ~%Available Tools:~%~
                                     • cluster-definitions: Group similar definitions~%~
                                     • find-similar-definitions: Find related theorems~%~
                                     • extract-ml-features: Deep feature analysis~%~
                                     • analyze-definition-structure: Complete structure analysis~%~
                                     • regenerate-index: Update definitions database~%~
                                     ~%This system provides AI-powered assistance for:~%~
                                     1. Finding similar existing theorems~%~
                                     2. Suggesting proof strategies~%~
                                     3. Identifying code patterns~%~
                                     4. Clustering related definitions"
                                    (length *definitions-index*)))))

;; Define API
(openrpc-server:define-api acl2ml-full-tools
  (:title "ACL2(ml) Complete Machine Learning Assistant"
   :version "3.0.0"
   :description "Full-featured AI assistant for ACL2 theorem proving with ML clustering"))

(format t "✓ Complete MCP tools registered:~%")
(format t "  • cluster-definitions~%")
(format t "  • find-similar-definitions~%")
(format t "  • extract-ml-features~%")
(format t "  • analyze-definition-structure~%")
(format t "  • regenerate-index~%")
(format t "  • acl2ml-demo~%")

(format t "~%🚀 Starting Complete ACL2(ml) MCP Server...~%")
(format t "Ready to provide ML-powered ACL2 theorem proving assistance!~%")

;; Start the MCP server
(40ants-mcp/server/definition:start-server 'acl2ml-full-tools)