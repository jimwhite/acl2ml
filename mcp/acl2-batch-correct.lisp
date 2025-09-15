;;;; acl2-batch-correct.lisp
;;;; Correct approach using ACL2's parser via saved_acl2

;; Load extraction functions
(load "extraction.lisp")
(load "table-to-feature-vector.lisp")

(defpackage #:acl2-batch-correct
  (:use #:cl #:acl2ml-complete-original)
  (:export #:batch-extract-using-acl2
           #:test-acl2-extraction))

(in-package :acl2-batch-correct)

(defun create-acl2-reader-script (lisp-file output-file)
  "Create ACL2 script to read forms and output them"
  (format nil ":q
(defun read-and-output-forms (input-file output-file)
  (with-open-file (input-stream input-file :direction :input)
    (with-open-file (output-stream output-file :direction :output :if-exists :supersede)
      (let ((forms nil))
        (handler-case
            (loop for form = (read input-stream nil :eof)
                  until (eq form :eof)
                  do (push form forms))
          (error (e)
            (format t \"Error reading ~A: ~A~%\" input-file e)
            (return-from read-and-output-forms nil)))
        (setf forms (reverse forms))
        (format output-stream \"(~%\")
        (dolist (form forms)
          (prin1 form output-stream)
          (terpri output-stream))
        (format output-stream \")~%\")
        (format t \"Processed ~A forms from ~A~%\" (length forms) input-file)
        (length forms)))))

(read-and-output-forms \"~A\" \"~A\")
(quit)
" lisp-file output-file))

(defun extract-using-acl2 (lisp-file)
  "Extract definitions from ACL2 file using proper ACL2 parsing"
  (let* ((safe-name (substitute #\_ #\/ lisp-file))
         (script-file (format nil "/tmp/script-~A.lsp" safe-name))
         (forms-file (format nil "/tmp/forms-~A.lsp" safe-name)))

    (unwind-protect
         (progn
           ;; Create the script
           (with-open-file (stream script-file :direction :output :if-exists :supersede)
             (write-string (create-acl2-reader-script lisp-file forms-file) stream))

           ;; Run ACL2
           (let ((exit-code (uiop:run-program
                            (format nil "timeout 30 saved_acl2 < ~A > /dev/null 2>&1" script-file)
                            :ignore-error-status t)))
             (declare (ignore exit-code))

             ;; Process the parsed forms if successful
             (when (probe-file forms-file)
               (handler-case
                   (with-open-file (stream forms-file :direction :input)
                     (let ((forms (read stream nil nil)))
                       (when forms
                         (extract-acl2-definitions-original-pipeline forms lisp-file))))
                 (error (e)
                   (format t "Error processing forms from ~A: ~A~%" lisp-file e)
                   nil)))))

      ;; Cleanup
      (ignore-errors (delete-file script-file))
      (ignore-errors (delete-file forms-file)))))

(defun test-acl2-extraction ()
  "Test extraction on a single file"
  (let ((test-file "/home/acl2/books/arithmetic-3/bind-free/basic.lisp"))
    (format t "Testing ACL2 extraction on ~A~%" test-file)
    (let ((definitions (extract-using-acl2 test-file)))
      (if definitions
          (format t "Successfully extracted ~A definitions~%" (length definitions))
        (format t "Extraction failed~%"))
      definitions)))

(defun find-certified-books (directory &optional (max-books 10))
  "Find certified ACL2 books"
  (let ((books nil)
        (count 0))
    (flet ((collect-books (path)
             (when (and (< count max-books)
                        (pathname-name path)
                        (string= (pathname-type path) "lisp"))
               (let ((cert-file (make-pathname :type "cert" :defaults path)))
                 (when (probe-file cert-file)
                   (push (namestring path) books)
                   (incf count))))))
      (uiop:collect-sub*directories
       directory
       (constantly t)
       (constantly t)
       #'collect-books))
    (reverse books)))

(defun batch-extract-using-acl2 ()
  "Batch extract using ACL2 - test with 10 books"
  (format t "ACL2ML Batch Processing (Correct ACL2 Approach)~%")
  (format t "===============================================~%")

  (let* ((books (find-certified-books "/home/acl2/books" 10))
         (successful 0)
         (failed 0))

    (format t "Testing with ~A books~%~%" (length books))
    (ensure-directories-exist "definitions/acl2-correct/")

    (loop for book in books
          for i from 1
          do (progn
               (format t "[~A/~A] Processing ~A...~%" i (length books) book)
               (let ((definitions (extract-using-acl2 book)))
                 (if definitions
                     (let* ((safe-name (substitute #\_ #\/ book))
                            (output-file (format nil "definitions/acl2-correct/~A" safe-name)))
                       (with-open-file (stream output-file :direction :output :if-exists :supersede)
                         (dolist (def definitions)
                           (format stream "~S~%" def)))
                       (format t "  → Generated ~A definitions~%~%" (length definitions))
                       (incf successful))
                   (progn
                     (format t "  → Failed~%~%")
                     (incf failed))))))

    (format t "BATCH TEST COMPLETE~%")
    (format t "===================~%")
    (format t "Successful: ~A/~A (~,1F%)~%"
            successful (length books) (* 100.0 (/ successful (length books))))
    (format t "Generated files in definitions/acl2-correct/~%")))