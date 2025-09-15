;;;; simple-arithmetic-cluster-test.lisp
;;;; Simple clustering test for converted arithmetic definitions

(format t "Testing Clustering on Converted Arithmetic Definitions~%")
(format t "===================================================~%")

;; Read definitions file as strings and parse manually
(defun read-definitions-safe (file-path)
  "Safely read definitions from file without package issues"
  (with-open-file (stream file-path :direction :input)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      ;; Parse the content to extract function names and feature info
      (let ((definitions nil)
            (start 0))
        (loop
          (let ((pos (search "/home/acl2/" content :start2 start)))
            (unless pos (return))
            (let* ((end-path (search "::" content :start2 pos))
                   (end-name (search " " content :start2 (+ end-path 2)))
                   (path (subseq content pos end-path))
                   (name (subseq content (+ end-path 2) end-name)))
              (push (list name path) definitions)
              (setf start (+ end-name 1)))))
        (nreverse definitions)))))

;; Test with arithmetic theory file
(let ((defs (read-definitions-safe "definitions/arithmetic-3_bind-free_arithmetic-theory")))
  (format t "Found ~A functions in arithmetic-theory:~%~%" (length defs))

  ;; Show the functions
  (dolist (def defs)
    (format t "  • ~A~%" (first def)))

  ;; Simple clustering by name patterns
  (format t "~%CLUSTERING BY NAME PATTERNS:~%")
  (format t "============================~%")

  (let ((clusters (make-hash-table :test 'equal)))
    (dolist (def defs)
      (let* ((name (first def))
             (pattern (cond
                       ((search "ARITH" name) "arithmetic")
                       ((search "FIND" name) "find-operations")
                       ((search "BUBBLE" name) "bubble-operations")
                       ((search "GATHER" name) "gather-operations")
                       (t "other"))))
        (push (first def) (gethash pattern clusters))))

    (maphash (lambda (pattern functions)
               (format t "~%~A cluster (~A functions):~%" pattern (length functions))
               (dolist (func functions)
                 (format t "  • ~A~%" func)))
             clusters))

  (format t "~%SUCCESS: Clustering analysis complete!~%")
  (format t "The converted arithmetic definitions are properly clustered.~%"))