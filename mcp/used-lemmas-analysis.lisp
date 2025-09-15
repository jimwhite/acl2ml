;;;; used-lemmas-analysis.lisp
;;;; ACL2(ml) Used Lemmas Analysis
;;;;
;;;; ORIGINAL MAPPING:
;;;; - code/used-lemmas.el:what-rewrite-rules-are-used-in() → analyze-theorem-rewrite-rules()
;;;; - code/used-lemmas.el:rewrite-runes() → extract-rewrite-runes()
;;;; - code/used-lemmas.el:see-definition() → extract-theorem-definition()
;;;; - code/used-lemmas.el:see-definition2() → extract-theorem-from-buffer()
;;;;
;;;; This analyzes ACL2 proof output to identify which lemmas/rules are used

(defpackage #:acl2ml-used-lemmas
  (:use #:cl)
  (:export #:analyze-theorem-rewrite-rules
           #:extract-rewrite-runes
           #:extract-theorem-definition
           #:analyze-lemma-usage
           #:format-lemma-analysis
           #:test-lemma-analysis))

(in-package :acl2ml-used-lemmas)

;;; Text parsing utilities
;; ORIGINAL: used-lemmas.el:remove-last-parenthesis() lines 37-43
;; CONVERTED: Same name, same functionality for cleaning parentheses
(defun remove-last-parenthesis (text)
  "Remove trailing parentheses from text (matches original exactly)
   ORIGINAL: used-lemmas.el:remove-last-parenthesis()
   CONVERTED: Pure Common Lisp version, same logic"
  (let ((clean-text (string-trim " " text)))
    (loop while (and (> (length clean-text) 0)
                    (char= (char clean-text (1- (length clean-text))) #\)))
          do (setf clean-text (subseq clean-text 0 (1- (length clean-text)))))
    clean-text))

;;; Rewrite rule extraction
;; ORIGINAL: used-lemmas.el:rewrite-runes() lines 25-33
;; CONVERTED: extract-rewrite-runes() - same name, same functionality
(defun extract-rewrite-runes (proof-text)
  "Extract :REWRITE rules from ACL2 proof output (mirrors original exactly)
   ORIGINAL: used-lemmas.el:rewrite-runes()
   CONVERTED: Pure Common Lisp version, same logic for parsing :REWRITE entries"
  (let ((rewrite-rules nil)
        (search-text proof-text))

    (loop
      (let ((rewrite-pos (search ":REWRITE" search-text)))
        (if rewrite-pos
            (let* ((line-start (+ rewrite-pos 9)) ; Skip ":REWRITE"
                   (line-end (or (position #\Newline search-text :start line-start)
                               (length search-text)))
                   (raw-rule (subseq search-text line-start line-end))
                   (clean-rule (remove-last-parenthesis (string-trim " " raw-rule))))
              (when (> (length clean-rule) 0)
                (push clean-rule rewrite-rules))
              (setf search-text (subseq search-text (1+ rewrite-pos))))
            (return))))

    (reverse rewrite-rules)))

;;; Main theorem analysis
;; ORIGINAL: used-lemmas.el:what-rewrite-rules-are-used-in() lines 15-21
;; CONVERTED: analyze-theorem-rewrite-rules() - core functionality
(defun analyze-theorem-rewrite-rules (theorem-name proof-output)
  "Analyze which rewrite rules are used in a theorem proof
   ORIGINAL: used-lemmas.el:what-rewrite-rules-are-used-in()
   CONVERTED: Parse ACL2 proof output to identify used lemmas

   ORIGINAL LOGIC:
   1. Search for (DEFTHM theorem-name in proof output
   2. Find Rules: section after theorem
   3. Find Time: section to mark end
   4. Extract rules from that section
   5. Parse :REWRITE entries

   CONVERTED: Same logic with Common Lisp string operations"
  (let* ((theorem-pattern (format nil "( DEFTHM ~A" (string-upcase (string theorem-name))))
         (theorem-pos (search theorem-pattern proof-output)))

    (if theorem-pos
        (let* ((rules-pos (search "Rules: " proof-output :start2 theorem-pos))
               (time-pos (search "Time: " proof-output :start2 theorem-pos)))

          (if (and rules-pos time-pos)
              (let ((rules-section (subseq proof-output rules-pos time-pos)))
                (list :theorem theorem-name
                      :rewrite-rules (extract-rewrite-runes rules-section)
                      :rules-found (length (extract-rewrite-runes rules-section))))
              (list :theorem theorem-name
                    :error "Could not locate Rules: or Time: sections")))

        (list :theorem theorem-name
              :error "Theorem not found in proof output"))))

;;; Theorem definition extraction
;; ORIGINAL: used-lemmas.el:see-definition() lines 47-60
;; CONVERTED: extract-theorem-definition() - same name, same functionality
(defun extract-theorem-definition (theorem-name proof-output)
  "Extract theorem definition from ACL2 proof output
   ORIGINAL: used-lemmas.el:see-definition()
   CONVERTED: Find and extract theorem statement from proof logs

   ORIGINAL LOGIC:
   1. Search for (DEFTHM theorem-name backward from end
   2. Also try multiline format with whitespace
   3. Find END-THM marker
   4. Extract theorem text between markers

   CONVERTED: Same logic using Common Lisp string operations"
  (let* ((pattern1 (format nil "(DEFTHM ~A" (string theorem-name)))
         (pattern2 (format nil "(DEFTHM~%               ~A" (string theorem-name)))
         ;; Search from end (reversed string search simulation)
         (def-pos1 (search pattern1 proof-output :from-end t))
         (def-pos2 (search pattern2 proof-output :from-end t)))

    (let ((start-pos (or def-pos1 def-pos2)))
      (if start-pos
          (let ((end-pos (search "END-THM" proof-output :start2 start-pos)))
            (if end-pos
                (let* ((theorem-text (subseq proof-output start-pos end-pos))
                       (last-paren (position #\) theorem-text :from-end t)))
                  (if last-paren
                      (string-trim '(#\Space #\Newline #\Tab)
                                  (subseq theorem-text 0 last-paren))
                      theorem-text))
                (format nil "END-THM not found for theorem ~A" theorem-name)))
          (format nil "Theorem ~A not found in proof output" theorem-name)))))

;;; High-level lemma usage analysis
;; ORIGINAL: Combines functionality from multiple functions in used-lemmas.el
;; CONVERTED: analyze-lemma-usage() - comprehensive analysis interface
(defun analyze-lemma-usage (theorem-name proof-output-file)
  "Comprehensive lemma usage analysis for a theorem
   ORIGINAL: Combination of used-lemmas.el functions
   CONVERTED: High-level interface that combines all analysis functions

   This provides the main functionality that the original system offered:
   1. Identify used rewrite rules
   2. Extract theorem definition
   3. Provide analysis summary"
  (handler-case
      (let* ((proof-text (with-open-file (stream proof-output-file :direction :input)
                          (with-output-to-string (s)
                            (loop for line = (read-line stream nil nil)
                                  while line
                                  do (write-line line s)))))
             (rewrite-analysis (analyze-theorem-rewrite-rules theorem-name proof-text))
             (theorem-def (extract-theorem-definition theorem-name proof-text)))

        (list :theorem theorem-name
              :proof-file proof-output-file
              :rewrite-rules (getf rewrite-analysis :rewrite-rules)
              :rules-count (getf rewrite-analysis :rules-found)
              :definition theorem-def
              :analysis-summary (format nil "Found ~A rewrite rules used in ~A"
                                       (getf rewrite-analysis :rules-found)
                                       theorem-name)))
    (error (e)
      (list :theorem theorem-name
            :error (format nil "Error analyzing lemma usage: ~A" e)))))

;;; Output formatting
;; ORIGINAL: used-lemmas.el had implicit formatting in the interactive functions
;; CONVERTED: format-lemma-analysis() - explicit formatting for MCP output
(defun format-lemma-analysis (analysis-result &key (stream t))
  "Format lemma usage analysis for display
   ORIGINAL: Implicit formatting in used-lemmas.el interactive functions
   CONVERTED: Explicit formatting for MCP tool output"
  (let ((theorem-name (getf analysis-result :theorem))
        (rules (getf analysis-result :rewrite-rules))
        (rules-count (getf analysis-result :rules-count))
        (definition (getf analysis-result :definition))
        (error-msg (getf analysis-result :error)))

    (with-output-to-string (s)
      (format s "~%LEMMA USAGE ANALYSIS~%")
      (format s "====================~%")
      (format s "Theorem: ~A~%~%" theorem-name)

      (if error-msg
          (format s "Error: ~A~%" error-msg)
          (progn
            (format s "Rewrite Rules Used (~A total):~%" rules-count)
            (if rules
                (dolist (rule rules)
                  (format s "  • ~A~%" rule))
                (format s "  (No rewrite rules found)~%"))

            (format s "~%Theorem Definition:~%")
            (format s "~A~%" definition)

            (format s "~%Analysis Summary:~%")
            (format s "• This theorem uses ~A different rewrite rules~%" rules-count)
            (format s "• Complexity indicator: ~A~%"
                    (cond
                      ((< rules-count 3) "Low dependency")
                      ((< rules-count 8) "Medium dependency")
                      (t "High dependency")))
            (format s "• Use this information for proof strategy and lemma selection~%"))))))

;;; Integration with ACL2 proof process
;; ORIGINAL: used-lemmas.el worked with tmp-file variable and ACL2 buffers
;; CONVERTED: analyze-current-proof() - integration with ACL2 session
(defun analyze-current-proof (theorem-name &optional (output-file "/tmp/acl2-proof-output.txt"))
  "Analyze lemma usage in current ACL2 proof session
   ORIGINAL: used-lemmas.el integration with ACL2 buffer and tmp-file
   CONVERTED: File-based integration with ACL2 proof output

   This would typically be called after running a proof in ACL2 that
   generates output to a temporary file"
  (if (probe-file output-file)
      (let ((analysis (analyze-lemma-usage theorem-name output-file)))
        (format-lemma-analysis analysis))
      (format nil "Proof output file not found: ~A~%Please ensure ACL2 proof output is saved to this file." output-file)))

;; Example usage and testing function
(defun test-lemma-analysis ()
  "Test the lemma analysis functionality with sample data"
  (let ((sample-proof-output "
(DEFTHM TEST-THEOREM
  (IMPLIES (AND (NATP X) (NATP Y))
           (EQUAL (+ X Y) (+ Y X)))

Rules: :REWRITE COMMUTATIVITY-OF-+
       :REWRITE NATP-COMPOUND-RECOGNIZER
       :REWRITE DEFAULT-+-1

Time: 0.02 seconds
END-THM
"))

    (format t "~%TESTING LEMMA ANALYSIS FUNCTIONALITY~%")
    (format t "====================================~%")

    ;; Test rewrite rule extraction
    (format t "~%Test 1: Rewrite rule extraction...~%")
    (let ((rules (extract-rewrite-runes sample-proof-output)))
      (format t "Found ~A rules: ~{~A~^, ~}~%" (length rules) rules))

    ;; Test theorem analysis
    (format t "~%Test 2: Complete theorem analysis...~%")
    (let ((analysis (analyze-theorem-rewrite-rules 'test-theorem sample-proof-output)))
      (format t "Analysis result: ~A~%" analysis))

    ;; Test definition extraction
    (format t "~%Test 3: Definition extraction...~%")
    (let ((definition (extract-theorem-definition 'test-theorem sample-proof-output)))
      (format t "Definition: ~A~%" definition))

    (format t "~%Lemma analysis test completed.~%")))