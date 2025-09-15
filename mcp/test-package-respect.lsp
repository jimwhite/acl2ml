:q
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")
(use-package :acl2ml-complete-original)

(defun read-acl2-forms-with-package-respect (filename)
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
                           (setf *package* pkg))))))
          (format t "Final package after reading: ~A~%" (package-name *package*)))
        (reverse forms))
    (error (e)
      (format t "Error reading ~A: ~A~%" filename e)
      nil)))

(defun test-package-respect ()
  "Test the package respecting approach"
  (let ((failing-book "/home/acl2/books/arithmetic-2/pass1/basic-arithmetic.lisp"))
    (format t "Testing package respect on: ~A~%" failing-book)

    (let ((forms (read-acl2-forms-with-package-respect failing-book)))
      (format t "Read ~A forms~%" (length forms))

      ;; Test extraction
      (let ((definitions (extract-acl2-definitions-original-pipeline forms failing-book)))
        (format t "Extracted ~A definitions~%"
                (if definitions (length definitions) 0))))))

(test-package-respect)
(quit)