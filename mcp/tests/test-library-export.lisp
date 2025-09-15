;;;; test-library-export.lisp
;;;; Test the library export functionality
;;;;
;;;; This tests the conversion from storage.el:export-library()

:q
(load "~/quicklisp/setup.lisp")

;; Load the library export system
(load "/workspaces/acl2ml/mcp/library-export.lisp")

(in-package :acl2ml-library-export)

;; Test data - sample definitions like those from our example.lisp
(defparameter *test-definitions*
  '((theta_sum defun 1 36)
    (helper_sum defun 2 45)
    (fn_sum defun 1 12)
    (theta_fact defun 1 38)
    (helper_fact defun 2 47)
    (fn_fact defun 1 12)))

(defun test-library-export ()
  "Test the library export functionality"
  (format t "~%TESTING LIBRARY EXPORT FUNCTIONALITY~%")
  (format t "====================================~%")

  ;; Test 1: Basic library export
  (format t "~%Test 1: Basic library export...~%")
  (let ((result (export-library "test-lib" *test-definitions*)))
    (format t "Exported ~A definitions~%" (length result))
    (format t "First definition: ~A~%" (first result)))

  ;; Test 2: Verify file was created
  (format t "~%Test 2: Verify definitions file exists...~%")
  (let ((def-file "/workspaces/acl2ml/mcp/definitions/test-lib"))
    (if (probe-file def-file)
        (progn
          (format t "✓ Definitions file created successfully~%")
          ;; Test 3: Import the definitions back
          (format t "~%Test 3: Import definitions back...~%")
          (let ((imported (import-definitions "test-lib")))
            (if imported
                (format t "✓ Successfully imported ~A definitions~%" (length imported))
                (format t "✗ Failed to import definitions~%"))))
        (format t "✗ Definitions file not found~%")))

  ;; Test 4: Test library name processing
  (format t "~%Test 4: Test library namespacing...~%")
  (let ((namespaced (add-libraryname-to-definitions "example" *test-definitions*)))
    (format t "Original: ~A~%" (first *test-definitions*))
    (format t "Namespaced: ~A~%" (first namespaced)))

  ;; Test 5: Test with example.lisp if it exists
  (format t "~%Test 5: Test with real ACL2 file...~%")
  (let ((example-file "/workspaces/acl2ml/manual/example.lisp"))
    (if (probe-file example-file)
        (handler-case
            (let ((result (generate-definitions-file example-file "example-test")))
              (if result
                  (format t "✓ Generated definitions for example.lisp: ~A items~%" (length result))
                  (format t "✗ No definitions extracted from example.lisp~%")))
          (error (e)
            (format t "✗ Error processing example.lisp: ~A~%" e)))
        (format t "✗ example.lisp not found~%")))

  (format t "~%Library export test completed.~%"))

;; Run the test
(test-library-export)