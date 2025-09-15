;;;; process-extracted-forms.lisp
;;;; Process forms extracted by ACL2 using our feature extraction

;; Load our extraction functions
(load "extraction.lisp")
(load "table-to-feature-vector.lisp")

(in-package :acl2ml-complete-original)

(defun process-forms-file (forms-file book-path)
  "Process forms from ACL2 extraction and generate definitions"
  (with-open-file (stream forms-file :direction :input)
    (let ((forms (read stream nil nil)))
      (when forms
        (extract-acl2-definitions-original-pipeline forms book-path)))))

;; Test with the extracted forms
(let ((definitions (process-forms-file "/tmp/basic-forms.lsp"
                                       "/home/acl2/books/arithmetic-3/bind-free/basic.lisp")))
  (if definitions
      (progn
        (format t "Successfully extracted ~A definitions~%" (length definitions))
        ;; Show first few definitions
        (dotimes (i (min 3 (length definitions)))
          (format t "Definition ~A: ~S~%" (1+ i) (nth i definitions))))
    (format t "No definitions extracted~%")))