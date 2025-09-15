;;;; acl2-batch-extractor.lisp
;;;; Correct approach: Use ACL2 to parse files, then process with Common Lisp
;;;;
;;;; This replaces the incorrect direct Common Lisp parsing approach

(defpackage #:acl2-batch-extractor
  (:use #:cl)
  (:export #:extract-using-acl2
           #:batch-extract-with-acl2))

(in-package :acl2-batch-extractor)

;;; Load our feature extraction functions
(load "table-to-feature-vector.lisp")
(use-package :acl2ml-complete-original)

(defun create-acl2-reader-script (lisp-file)
  "Create a script that uses ACL2's reader to parse forms, then switches to CL"
  (let ((temp-script (format nil "/tmp/read-forms-~A.lsp"
                             (substitute #\_ #\/ lisp-file)))
        (temp-output (format nil "/tmp/parsed-forms-~A.lsp"
                             (substitute #\_ #\/ lisp-file))))

    ;; Create the ACL2 script
    (with-open-file (stream temp-script :direction :output :if-exists :supersede)
      (format stream ":q
; Now in Common Lisp - can use CL functions to read ACL2 file
(defun read-acl2-file-forms (filename)
  \"Read all forms from ACL2 file using ACL2's reader\"
  (with-open-file (stream filename :direction :input)
    (let ((forms nil))
      (loop for form = (read stream nil :eof)
            until (eq form :eof)
            do (push form forms))
      (reverse forms))))

(defun write-forms-for-cl (forms filename)
  \"Write forms in a format Common Lisp can easily process\"
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream \"(~%\")
    (dolist (form forms)
      (format stream \"~S~%\" form))
    (format stream \")~%\")))

; Read the forms and write them out
(let ((forms (ignore-errors (read-acl2-file-forms \"~A\"))))
  (if forms
      (progn
        (write-forms-for-cl forms \"~A\")
        (format t \"Successfully processed ~A forms from ~A~%\"
                (length forms) \"~A\"))
    (format t \"Error reading forms from ~A~%\" \"~A\")))

; Exit Common Lisp completely
(quit)
" lisp-file temp-output lisp-file lisp-file lisp-file))

    (values temp-script temp-output)))

(defun extract-using-acl2 (lisp-file)
  "Use ACL2 to parse an ACL2 .lisp file and extract definitions"
  (multiple-value-bind (temp-script temp-output)
      (create-acl2-reader-script lisp-file)

    (unwind-protect
         ;; Run ACL2 with the script
         (let ((exit-code (uiop:run-program
                          (format nil "cd /home/acl2 && timeout 30 ./saved_acl2 < ~A > /dev/null 2>&1"
                                  temp-script)
                          :ignore-error-status t)))
           (declare (ignore exit-code))

           ;; If ACL2 succeeded, process the output
           (when (probe-file temp-output)
             (handler-case
                 (with-open-file (stream temp-output :direction :input)
                   (let ((forms (read stream nil nil)))
                     (when forms
                       ;; Now use our extraction pipeline on the properly parsed forms
                       (extract-acl2-definitions-original-pipeline forms lisp-file))))
               (error (e)
                 (format t "Error processing forms from ~A: ~A~%" lisp-file e)
                 nil))))

      ;; Cleanup
      (ignore-errors (delete-file temp-script))
      (ignore-errors (delete-file temp-output)))))

(defun batch-extract-with-acl2 ()
  "Batch extract using ACL2 for parsing"
  (format t "ACL2ML Batch Processing (Using ACL2 Parser)~%")
  (format t "==========================================~%")

  (let* ((acl2-books-dir "/home/acl2/books")
         (books (find-certified-acl2-books acl2-books-dir))
         (total-books (length books))
         (successful 0)
         (failed 0))

    (format t "Found ~A certified ACL2 books~%" total-books)
    (format t "Processing with ACL2 parser...~%~%")

    ;; Create output directory
    (ensure-directories-exist "definitions/acl2-parsed/")

    ;; Process first 10 books as a test
    (loop for book-path in (subseq books 0 (min 10 total-books))
          for i from 1
          do (progn
               (format t "[~A/~A] Processing ~A...~%"
                       i (min 10 total-books) book-path)

               (let ((definitions (extract-using-acl2 book-path)))
                 (if definitions
                     (let* ((safe-name (substitute #\_ #\/ book-path))
                            (output-file (format nil "definitions/acl2-parsed/~A" safe-name)))
                       (with-open-file (stream output-file :direction :output
                                               :if-exists :supersede)
                         (dolist (def definitions)
                           (format stream "~S~%" def)))
                       (format t "  → Generated ~A definitions in ~A~%~%"
                               (length definitions) output-file)
                       (incf successful))
                   (progn
                     (format t "  → Failed~%~%")
                     (incf failed))))))

    (format t "~%TEST BATCH COMPLETE~%")
    (format t "====================~%")
    (format t "Processed: ~A books~%" (min 10 total-books))
    (format t "Successful: ~A books~%" successful)
    (format t "Failed: ~A books~%" failed)
    (format t "Success rate: ~,1F%~%" (* 100.0 (/ successful (min 10 total-books))))
    (format t "Generated files in definitions/acl2-parsed/~%")))

(defun find-certified-acl2-books (directory)
  "Find all certified ACL2 books (with .cert files)"
  (let ((books nil))
    (flet ((collect-books (path)
             (when (and (pathname-name path)
                        (string= (pathname-type path) "lisp"))
               (let ((cert-file (make-pathname :type "cert" :defaults path)))
                 (when (probe-file cert-file)
                   (push (namestring path) books))))))
      (uiop:collect-sub*directories
       directory
       (constantly t)
       (constantly t)
       #'collect-books))
    (reverse books)))