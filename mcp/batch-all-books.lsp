:q
; Now in Common Lisp mode in saved_acl2
; Load our extraction functions
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")
(use-package :acl2ml-complete-original)


(defun find-certified-books (directory &optional max-books)
  "Find all certified ACL2 books"
  (let ((books nil)
        (count 0))
    (labels ((visit-directory (dir)
               (when (probe-file dir)
                 (dolist (entry (directory (merge-pathnames "*.*" dir)))
                   (cond
                     ((and (null max-books) (>= count 1000))
                      (return-from find-certified-books (reverse books)))
                     ((and max-books (>= count max-books))
                      (return-from find-certified-books (reverse books)))
                     ((and (pathname-name entry)
                           (string= (pathname-type entry) "lisp"))
                      (let ((cert-file (make-pathname :type "cert" :defaults entry)))
                        (when (probe-file cert-file)
                          (push (namestring entry) books)
                          (incf count))))
                     ((and (not (pathname-name entry))
                           (not (pathname-type entry)))
                      (visit-directory entry)))))))
      (visit-directory (pathname directory)))
    (reverse books)))

(defun read-acl2-forms (filename)
  "Read all forms from an ACL2 file using ACL2's reader"
  (handler-case
      (let ((*package* (find-package "ACL2"))) ; Switch to ACL2 package for reading
        (with-open-file (stream filename :direction :input)
          (let ((forms nil))
            (loop for form = (read stream nil :eof)
                  until (eq form :eof)
                  do (push form forms))
            (reverse forms))))
    (error (e)
      (with-open-file (log "/workspaces/acl2ml/mcp/batch-errors.log"
                           :direction :output :if-exists :append :if-does-not-exist :create)
        (format log "[READ ERROR] ~A: ~A~%" filename e))
      (format t "Error reading ~A: ~A~%" filename e)
      nil)))

(defun process-single-book (book-path)
  "Process a single ACL2 book and return definitions"
  (let ((forms (read-acl2-forms book-path)))
    (when forms
      (handler-case
          (extract-acl2-definitions-original-pipeline forms book-path)
        (error (e)
          (with-open-file (log "/workspaces/acl2ml/mcp/batch-errors.log"
                               :direction :output :if-exists :append :if-does-not-exist :create)
            (format log "[PROCESS ERROR] ~A: ~A~%" book-path e))
          (format t "Error processing ~A: ~A~%" book-path e)
          nil)))))

(defun batch-process-all-books ()
  "Process all certified ACL2 books"
  (format t "ACL2ML Batch Processing (Single ACL2 Session)~%")
  (format t "============================================~%")

  (let* ((books (find-certified-books "/home/acl2/books" 50))  ; Test with more books
         (total (length books))
         (successful 0)
         (failed 0)
         (output-dir "/workspaces/acl2ml/mcp/definitions/acl2-single-session/"))

    (format t "Found ~A books to process~%~%" total)

    ;; Ensure output directory exists
    (ensure-directories-exist output-dir)

    (loop for book in books
          for i from 1
          do (progn
               (format t "[~A/~A] Processing ~A...~%" i total book)

               (let ((definitions (process-single-book book)))
                 (if definitions
                     (let* ((safe-name (substitute #\_ #\/ book))
                            (output-file (format nil "~A~A" output-dir safe-name)))
                       (with-open-file (stream output-file :direction :output
                                               :if-exists :supersede)
                         (dolist (def definitions)
                           (format stream "~S~%" def)))
                       (format t "  → Generated ~A definitions~%~%"
                               (length definitions))
                       (incf successful))
                   (progn
                     (format t "  → Failed~%~%")
                     (incf failed))))))

    (format t "BATCH PROCESSING COMPLETE~%")
    (format t "=========================~%")
    (format t "Total books: ~A~%" total)
    (format t "Successful: ~A~%" successful)
    (format t "Failed: ~A~%" failed)
    (format t "Success rate: ~,1F%~%" (* 100.0 (/ successful total)))
    (format t "Output directory: ~A~%" output-dir)))

; Run the batch processing
(batch-process-all-books)

; Exit
(quit)