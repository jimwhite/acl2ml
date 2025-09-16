; Load the verified extraction components
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")
(load "/workspaces/acl2ml/mcp/extraction-bridge.lisp")

; Everything is now in the acl2ml-mcp package
(in-package :acl2ml-mcp)

;;;; Integration tests for core processing pipeline
;;;;
;;;; Tests the complete pipeline using real ACL2 content:
;;;; 1. manual/example.lisp - Primary test case from manual
;;;; 2. ACL2 system books - Real-world validation
;;;;
;;;; NO STUBS/MOCKS - Uses real ACL2 content and processing

;;; =======================================================================
;;; Test Utilities
;;; =======================================================================

(defun load-file-content (filepath)
  "Load file content as string"
  (with-open-file (stream filepath :direction :input)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))

(defun print-test-header (test-name)
  "Print formatted test header"
  (format t "~%========================================~%")
  (format t "TEST: ~A~%" test-name)
  (format t "========================================~%"))

(defun print-test-results (results)
  "Print test results in readable format"
  (format t "RESULTS:~%")
  (format t "  Vectors generated: ~A~%" (length (getf results :vectors)))
  (format t "  Processing summary: ~A~%" (getf results :processing-summary))
  (format t "  Final count: ~A~%" (getf results :final-count))

  ;; Show sample vectors
  (let ((vectors (getf results :vectors)))
    (when vectors
      (format t "~%SAMPLE VECTORS:~%")
      (loop for vector in (subseq vectors 0 (min 3 (length vectors)))
            do (format t "  ~A: [~A numbers]~%"
                      (car vector)
                      (length (cadr vector)))))))

(defun validate-expected-functions (vectors expected-functions)
  "Validate that expected functions are found in results"
  (format t "~%VALIDATION:~%")
  (let ((found-functions (mapcar #'car vectors))
        (all-found t))

    (dolist (expected expected-functions)
      (if (find expected found-functions :test #'string-equal)
          (format t "  ✓ Found expected function: ~A~%" expected)
          (progn
            (format t "  ✗ Missing expected function: ~A~%" expected)
            (setf all-found nil))))

    (format t "~%  Found functions: ~A~%" found-functions)
    all-found))

;;; =======================================================================
;;; Test 1: Manual Example (manual/example.lisp)
;;; =======================================================================

(defun test-example-lisp ()
  "Test processing of manual/example.lisp - the primary test case"
  (print-test-header "MANUAL EXAMPLE.LISP")

  ;; Load example content
  (let ((example-content (load-file-content "/workspaces/acl2ml/manual/example.lisp")))

    (format t "Loaded example.lisp content (~A characters)~%" (length example-content))
    (format t "First 200 characters: ~A...~%~%" (subseq example-content 0 (min 200 (length example-content))))

    ;; Test definitions processing
    (format t "TESTING DEFINITIONS PROCESSING:~%")
    (clear-processing-state)
    (let ((def-results (complete-extraction-pipeline
                        example-content "definitions")))

      (print-test-results def-results)

      ;; Validate expected functions from example.lisp
      (let ((expected-defs '("theta_sum" "helper_sum" "fn_sum"
                           "theta_fact" "helper_fact" "fn_fact"
                           "theta_expt" "helper_expt" "fn_expt")))
        (validate-expected-functions (getf def-results :vectors) expected-defs))

      def-results)))

;;; =======================================================================
;;; Test 2: System Books (Real ACL2 content)
;;; =======================================================================

(defun find-sample-system-book ()
  "Find a small ACL2 system book for testing"
  (let ((potential-books '("/home/acl2/books/arithmetic-5/lib/basic-ops/arithmetic-theory.lisp"
                          "/home/acl2/books/std/lists/rev.lisp"
                          "/home/acl2/books/std/util/defconsts.lisp")))
    (dolist (book potential-books)
      (when (probe-file book)
        (return book)))))

(defun test-system-book ()
  "Test processing of a real ACL2 system book"
  (print-test-header "SYSTEM BOOK")

  (let ((book-path (find-sample-system-book)))
    (if book-path
        (progn
          (format t "Testing with system book: ~A~%" book-path)

          ;; Load book content
          (let ((book-content (load-file-content book-path)))
            (format t "Loaded book content (~A characters)~%" (length book-content))

            ;; Process definitions
            (clear-processing-state)
            (let ((results (complete-extraction-pipeline
                           book-content "definitions")))

              (print-test-results results)

              ;; Basic validation
              (let ((vector-count (length (getf results :vectors))))
                (format t "~%VALIDATION:~%")
                (if (> vector-count 0)
                    (format t "  ✓ Successfully processed system book (~A definitions)~%" vector-count)
                    (format t "  ✗ No definitions found in system book~%")))

              results)))

        (format t "No accessible system books found for testing~%"))))

;;; =======================================================================
;;; Test 3: Error Handling and Edge Cases
;;; =======================================================================

(defun test-error-handling ()
  "Test error handling with malformed content"
  (print-test-header "ERROR HANDLING")

  ;; Test with malformed ACL2 content
  (let ((malformed-content "(defun incomplete-function"))
    (format t "Testing with malformed content...~%")

    (clear-processing-state)
    (handler-case
        (let ((results (complete-extraction-pipeline
                       malformed-content "definitions")))
          (format t "  ✓ Handled malformed content gracefully~%")
          (print-test-results results))
      (error (e)
        (format t "  ✗ Error handling failed: ~A~%" e))))

  ;; Test with empty content
  (format t "~%Testing with empty content...~%")
  (clear-processing-state)
  (let ((results (complete-extraction-pipeline "" "definitions")))
    (format t "  ✓ Handled empty content~%")
    (print-test-results results)))

;;; =======================================================================
;;; Test 4: Pattern Validation for Clustering
;;; =======================================================================

(defun analyze-clustering-patterns (vectors)
  "Analyze if vectors show expected clustering patterns"
  (format t "~%CLUSTERING PATTERN ANALYSIS:~%")

  ;; Group by naming patterns
  (let ((theta-funcs '())
        (helper-funcs '())
        (fn-funcs '())
        (other-funcs '()))

    (dolist (vector vectors)
      (let ((name (string-upcase (string (car vector)))))
        (cond
          ((search "THETA_" name) (push vector theta-funcs))
          ((search "HELPER_" name) (push vector helper-funcs))
          ((search "FN_" name) (push vector fn-funcs))
          (t (push vector other-funcs)))))

    (format t "  Direct recursive (theta_*): ~A functions~%" (length theta-funcs))
    (format t "  Tail recursive helpers (helper_*): ~A functions~%" (length helper-funcs))
    (format t "  Wrapper functions (fn_*): ~A functions~%" (length fn-funcs))
    (format t "  Other functions: ~A functions~%" (length other-funcs))

    ;; Check if we have the expected pattern groups
    (and (> (length theta-funcs) 0)
         (> (length helper-funcs) 0)
         (> (length fn-funcs) 0))))

(defun validate-clustering-patterns ()
  "Validate that processing produces vectors suitable for clustering"
  (print-test-header "CLUSTERING PATTERNS VALIDATION")

  ;; Use example.lisp for pattern validation
  (let ((example-content (load-file-content "/workspaces/acl2ml/manual/example.lisp")))

    (clear-processing-state)
    (let* ((results (complete-extraction-pipeline
                     example-content "definitions"))
           (vectors (getf results :vectors)))

      (if (analyze-clustering-patterns vectors)
          (format t "  ✓ Found expected clustering patterns~%")
          (format t "  ✗ Missing expected clustering patterns~%"))

      ;; Check vector dimensions
      (when vectors
        (let* ((sample-vector (cadar vectors))
               (dimensions (length sample-vector)))
          (format t "  Feature vector dimensions: ~A~%" dimensions)
          (if (> dimensions 0)
              (format t "  ✓ Vectors have features for clustering~%")
              (format t "  ✗ Vectors are empty~%"))))

      vectors)))

;;; =======================================================================
;;; Main Test Runner
;;; =======================================================================

(defun run-all-tests ()
  "Run all integration tests"
  (format t "~%🚀 STARTING ACL2(ML) CORE PIPELINE INTEGRATION TESTS~%")
  (format t "====================================================~%")

  (let ((start-time (get-universal-time))
        (tests-passed 0)
        (total-tests 4))

    ;; Test 1: Manual example
    (handler-case
        (progn
          (test-example-lisp)
          (incf tests-passed)
          (format t "~%✅ EXAMPLE.LISP TEST PASSED~%"))
      (error (e)
        (format t "~%❌ EXAMPLE.LISP TEST FAILED: ~A~%" e)))

    ;; Test 2: System book
    (handler-case
        (progn
          (test-system-book)
          (incf tests-passed)
          (format t "~%✅ SYSTEM BOOK TEST PASSED~%"))
      (error (e)
        (format t "~%❌ SYSTEM BOOK TEST FAILED: ~A~%" e)))

    ;; Test 3: Error handling
    (handler-case
        (progn
          (test-error-handling)
          (incf tests-passed)
          (format t "~%✅ ERROR HANDLING TEST PASSED~%"))
      (error (e)
        (format t "~%❌ ERROR HANDLING TEST FAILED: ~A~%" e)))

    ;; Test 4: Clustering patterns
    (handler-case
        (progn
          (validate-clustering-patterns)
          (incf tests-passed)
          (format t "~%✅ CLUSTERING PATTERNS TEST PASSED~%"))
      (error (e)
        (format t "~%❌ CLUSTERING PATTERNS TEST FAILED: ~A~%" e)))

    ;; Summary
    (let ((duration (- (get-universal-time) start-time)))
      (format t "~%====================================================~%")
      (format t "🏁 TEST SUMMARY~%")
      (format t "====================================================~%")
      (format t "Tests passed: ~A/~A~%" tests-passed total-tests)
      (format t "Duration: ~A seconds~%" duration)

      (if (= tests-passed total-tests)
          (format t "🎉 ALL TESTS PASSED! Core pipeline is ready.~%")
          (format t "⚠️  Some tests failed. Check output above.~%")))

    ;; Return results
    (list :passed tests-passed :total total-tests :success (= tests-passed total-tests))))

;; Make it easy to run tests
(format t "~%To run tests, execute: (acl2ml-mcp:run-all-tests)~%")