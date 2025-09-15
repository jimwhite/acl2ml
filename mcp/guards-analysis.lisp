;;;; guards-analysis.lisp
;;;; ACL2 Guard Extraction and Analysis
;;;;
;;;; ORIGINAL MAPPING:
;;;; - code/guards.el:obtain-guards-theorem() → extract-theorem-guards()
;;;; - code/guards.el:read-guards() → parse-guard-info()
;;;; - code/shortcuts.el:acl2ml-obtain-guards (C-c C-g) → MCP tool get-function-guards
;;;;
;;;; This converts the ACL2 guard extraction functionality from Emacs to Common Lisp

(defpackage #:acl2ml-guards
  (:use #:cl)
  (:export #:obtain-guards-theorem
           #:get-function-guards
           #:analyze-guard-conditions))

(in-package :acl2ml-guards)

;; ORIGINAL: code/guards.el:obtain-guards-theorem() lines 49+
;; CONVERTED: Same name, same functionality
(defun obtain-guards-theorem (theorem-expression)
  "Extract guards from ACL2 theorem or function definition
   ORIGINAL: code/guards.el:obtain-guards-theorem()
   CONVERTED: Parse ACL2 expression and identify guard conditions"
  (handler-case
      (let* ((expr (if (stringp theorem-expression)
                       (read-from-string theorem-expression)
                       theorem-expression))
             (guards (find-guard-conditions expr)))
        (list :theorem (second expr)
              :type (first expr)
              :guards guards
              :guard-count (length guards)))
    (error (e)
      (list :error (format nil "Error extracting guards: ~A" e)))))

;; ORIGINAL: code/guards.el implicit guard detection logic
;; CONVERTED: find-guard-conditions() - identify guard-like conditions
(defun find-guard-conditions (expr)
  "Find conditions that look like ACL2 guards
   ORIGINAL: Implicit in guards.el guard extraction
   CONVERTED: Pattern matching for common ACL2 guard patterns"
  (let ((guards nil))
    (labels ((scan-for-guards (form)
               (when (and (listp form) (> (length form) 1))
                 ;; Look for common guard patterns
                 (let ((head (first form)))
                   (cond
                     ;; Type predicates that are often guards
                     ((member head '(integerp natp posp rationalp consp atom listp))
                      (push form guards))
                     ;; Arithmetic comparisons
                     ((member head '(< > <= >= = /=))
                      (push form guards))
                     ;; Logical combinations - recurse
                     ((eq head 'and)
                      (dolist (arg (rest form))
                        (scan-for-guards arg)))
                     ((eq head 'or)
                      (dolist (arg (rest form))
                        (scan-for-guards arg)))
                     ;; Other conditions - check arguments
                     (t
                      (dolist (arg (rest form))
                        (when (listp arg)
                          (scan-for-guards arg)))))))))
      (scan-for-guards expr)
      (remove-duplicates guards :test #'equal))))

;; ORIGINAL: code/guards.el:read-guards() functionality
;; CONVERTED: analyze-guard-conditions() - analyze extracted guards
(defun analyze-guard-conditions (guards)
  "Analyze extracted guard conditions
   ORIGINAL: code/guards.el:read-guards() parsing logic
   CONVERTED: Categorize and analyze guard types"
  (let ((type-guards nil)
        (numeric-guards nil)
        (structural-guards nil)
        (other-guards nil))

    (dolist (guard guards)
      (when (and (listp guard) (> (length guard) 0))
        (let ((predicate (first guard)))
          (cond
            ;; Type predicates
            ((member predicate '(integerp natp posp rationalp consp atom listp
                                 symbolp stringp characterp))
             (push guard type-guards))
            ;; Numeric comparisons
            ((member predicate '(< > <= >= = /=))
             (push guard numeric-guards))
            ;; Structural predicates
            ((member predicate '(consp endp null))
             (push guard structural-guards))
            ;; Everything else
            (t
             (push guard other-guards))))))

    (list :type-guards type-guards
          :numeric-guards numeric-guards
          :structural-guards structural-guards
          :other-guards other-guards
          :total-guards (length guards))))

;; ORIGINAL: code/guards.el:nested-lists-to-string() - format for display
;; CONVERTED: format-guard-analysis() - format guard analysis results
(defun format-guard-analysis (theorem-name guards-info)
  "Format guard analysis for display
   ORIGINAL: code/guards.el:nested-lists-to-string() + display logic
   CONVERTED: Format guard analysis results for MCP output"
  (let ((analysis (analyze-guard-conditions (getf guards-info :guards))))
    (with-output-to-string (s)
      (format s "ACL2 Guard Analysis for ~A~%" theorem-name)
      (format s "=====================================~%~%")
      (format s "Theorem Type: ~A~%" (getf guards-info :type))
      (format s "Total Guards Found: ~A~%~%" (getf guards-info :guard-count))

      (when (getf analysis :type-guards)
        (format s "Type Guards:~%")
        (dolist (guard (getf analysis :type-guards))
          (format s "  • ~A~%" guard))
        (format s "~%"))

      (when (getf analysis :numeric-guards)
        (format s "Numeric Guards:~%")
        (dolist (guard (getf analysis :numeric-guards))
          (format s "  • ~A~%" guard))
        (format s "~%"))

      (when (getf analysis :structural-guards)
        (format s "Structural Guards:~%")
        (dolist (guard (getf analysis :structural-guards))
          (format s "  • ~A~%" guard))
        (format s "~%"))

      (when (getf analysis :other-guards)
        (format s "Other Conditions:~%")
        (dolist (guard (getf analysis :other-guards))
          (format s "  • ~A~%" guard))
        (format s "~%"))

      (format s "Guard Analysis:~%")
      (format s "• This function/theorem has ~A guard conditions~%"
              (getf guards-info :guard-count))
      (format s "• Guard complexity: ~A~%"
              (cond
                ((< (getf guards-info :guard-count) 3) "Low")
                ((< (getf guards-info :guard-count) 6) "Medium")
                (t "High")))
      (format s "• Use this information to understand preconditions~%")
      (format s "  and help with theorem proving strategies~%"))))

;; High-level interface function
;; ORIGINAL: code/shortcuts.el:acl2ml-obtain-guards (C-c C-g)
;; CONVERTED: get-function-guards() - main MCP interface
(defun get-function-guards (definition-text)
  "Get guards for ACL2 function or theorem (MCP interface)
   ORIGINAL: code/shortcuts.el:acl2ml-obtain-guards (C-c C-g)
   CONVERTED: Main interface for guard extraction via MCP"
  (handler-case
      (let* ((guards-info (obtain-guards-theorem definition-text))
             (theorem-name (or (getf guards-info :theorem) "unnamed"))
             (formatted-output (format-guard-analysis theorem-name guards-info)))
        formatted-output)
    (error (e)
      (format nil "Error analyzing guards: ~A" e))))

;; Example usage and testing
(defun test-guard-extraction ()
  "Test the guard extraction functionality"
  (let ((test-theorem "(defthm test-theorem
                        (implies (and (natp x) (> x 0) (consp lst))
                                (equal (+ x 1) (1+ x))))"))
    (format t "~A~%" (get-function-guards test-theorem))))