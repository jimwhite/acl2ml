;;; test-extraction.lisp
;;; Test functions for the ACL2ML extraction pipeline

(in-package :acl2ml-complete-original)

(defun test-complete-original-extraction ()
  "Test the complete original extraction system"
  (format t "~%TESTING COMPLETE ORIGINAL EXTRACTION~%")
  (format t "====================================~%")

  ;; Test the basic extraction components
  (let* ((test-defun '(defun test-fn (x) (if (consp x) (car x) nil)))
         (extracted-info (extract-info test-defun))
         (built-table (build-table extracted-info))
         (with-recursion (search-for-recursive-calls built-table)))

    (format t "Test DEFUN - ~A~%" test-defun)
    (format t "Extract-info - ~A~%" extracted-info)
    (format t "Build-table - ~A~%" built-table)
    (format t "With recursion - ~A~%" with-recursion))

  (format t "~%Complete original extraction test completed.~%"))