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

(defun read-acl2-forms-properly (filename)
  "Read all forms from an ACL2 file using proper ACL2 file reading"
  ;; Go back to ACL2 mode to use proper ACL2 file reading functions
  (format t ":q~%(lp)~%")  ; Go back to ACL2 mode

  ;; Use ACL2's proper file reading approach
  (let ((forms-var (gensym "FORMS")))
    (format t "(assign ~A nil)~%" forms-var)
    (format t "(mv-let (channel state)~%")
    (format t "  (open-input-channel \"~A\" :character state)~%" filename)
    (format t "  (if channel~%")
    (format t "      (mv-let (eof-read-p obj state)~%")
    (format t "        (read-object channel state)~%")
    (format t "        (mv-let (all-forms state)~%")
    (format t "          (read-all-objects-from-channel channel obj state)~%")
    (format t "          (mv-let (state)~%")
    (format t "            (close-input-channel channel state)~%")
    (format t "            (assign ~A all-forms))))~%" forms-var)
    (format t "    (mv state)))~%")
    (format t ":q~%")  ; Back to Common Lisp mode

    ;; Get the forms from the assigned variable
    (format t "(acl2::@ ~A)~%" forms-var)))

(defun read-acl2-forms (filename)
  "Read all forms from an ACL2 file respecting package changes"
  (handler-case
      (let ((*package* (find-package "ACL2")) ; Start in ACL2 package
            (forms nil))
        (with-open-file (stream filename :direction :input)
          (loop for form = (read stream nil :eof)
                until (eq form :eof)
                do (progn
                     (push form forms)
                     ;; If this is an in-package form, switch packages
                     (when (and (listp form)
                                (equal (first form) 'in-package)
                                (stringp (second form)))
                       (let ((pkg (find-package (second form))))
                         (when pkg
                           (setf *package* pkg)))))))
        (reverse forms))
    (error (e)
      (with-open-file (log "/workspaces/acl2ml/mcp/batch-errors.log"
                           :direction :output :if-exists :append :if-does-not-exist :create)
        (format log "[READ ERROR] ~A: ~A~%" filename e))
      (format t "Error reading ~A: ~A~%" filename e)
      nil)))

(defun process-single-book (book-path)
  "Process a single ACL2 book and return definitions"
  (let ((forms (read-acl2-forms book-path)))
    (if forms
        (handler-case
            (let ((result (extract-acl2-definitions-original-pipeline forms book-path)))
              (unless result
                ;; Log when no definitions found but no error
                (with-open-file (log "/workspaces/acl2ml/mcp/batch-errors.log"
                                     :direction :output :if-exists :append :if-does-not-exist :create)
                  (format log "[NO DEFINITIONS] ~A: Forms read: ~A~%" book-path (length forms))))
              result)
          (error (e)
            (with-open-file (log "/workspaces/acl2ml/mcp/batch-errors.log"
                                 :direction :output :if-exists :append :if-does-not-exist :create)
              (format log "[PROCESS ERROR] ~A: ~A~%" book-path e))
            (format t "Error processing ~A: ~A~%" book-path e)
            nil))
      ;; Log when forms reading failed
      (progn
        (with-open-file (log "/workspaces/acl2ml/mcp/batch-errors.log"
                             :direction :output :if-exists :append :if-does-not-exist :create)
          (format log "[READ FAILED] ~A: Could not read forms~%" book-path))
        nil))))

(defun batch-process-all-books ()
  "Process all certified ACL2 books"
  (format t "ACL2ML Batch Processing (Single ACL2 Session)~%")
  (format t "============================================~%")

  (let* ((books (find-certified-books "/home/acl2/books"))  ; Process all books
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