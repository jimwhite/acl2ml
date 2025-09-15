:q
; Test extraction directly on the forms
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")
(use-package :acl2ml-complete-original)

(defun test-extraction-directly ()
  "Test extraction on individual forms"
  (let ((book "/home/acl2/books/arithmetic-2/pass1/basic-arithmetic.lisp"))
    (format t "Testing extraction on individual DEFTHM forms~%")

    (with-open-file (stream book :direction :input)
      (let ((forms nil))
        (loop for form = (read stream nil :eof)
              until (eq form :eof)
              do (push form forms))
        (setf forms (reverse forms))

        ; Test first DEFTHM form
        (let ((first-defthm (find-if (lambda (form)
                                       (and (listp form)
                                            (equal (car form) 'defthm)))
                                     forms)))
          (when first-defthm
            (format t "First DEFTHM: ~S~%~%" first-defthm)

            ; Test extract-info function
            (format t "Testing extract-info...~%")
            (handler-case
                (let ((info (extract-info first-defthm)))
                  (format t "Extract-info result: ~S~%~%" info))
              (error (e)
                (format t "ERROR in extract-info: ~A~%~%" e)))

            ; Test build-table function
            (format t "Testing build-table...~%")
            (handler-case
                (let* ((info (extract-info first-defthm))
                       (table (build-table info)))
                  (format t "Build-table result: ~S~%~%" table))
              (error (e)
                (format t "ERROR in build-table: ~A~%~%" e)))

            ; Test full pipeline on this form
            (format t "Testing full pipeline on single form...~%")
            (handler-case
                (let ((result (extract-acl2-definitions-original-pipeline
                              (list first-defthm) book)))
                  (format t "Pipeline result: ~S~%~%" result))
              (error (e)
                (format t "ERROR in pipeline: ~A~%~%" e)))))))))

(test-extraction-directly)
(quit)