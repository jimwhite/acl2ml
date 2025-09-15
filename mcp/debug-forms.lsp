:q
; Debug version that shows what forms we're reading
(load "/workspaces/acl2ml/mcp/extraction.lisp")
(load "/workspaces/acl2ml/mcp/table-to-feature-vector.lisp")
(use-package :acl2ml-complete-original)

(defun analyze-forms ()
  "Analyze the forms we're reading from a book"
  (let ((book "/home/acl2/books/arithmetic-2/pass1/basic-arithmetic.lisp"))
    (format t "Analyzing forms from: ~A~%" book)

    (with-open-file (stream book :direction :input)
      (let ((forms nil)
            (form-types (make-hash-table :test 'equal)))
        (loop for form = (read stream nil :eof)
              until (eq form :eof)
              do (progn
                   (push form forms)
                   (let ((type (if (and (listp form) (symbolp (car form)))
                                   (symbol-name (car form))
                                   "OTHER")))
                     (incf (gethash type form-types 0)))))

        (setf forms (reverse forms))
        (format t "Total forms: ~A~%~%" (length forms))

        ; Show form type distribution
        (format t "Form types:~%")
        (maphash (lambda (type count)
                   (format t "  ~A: ~A~%" type count))
                 form-types)

        ; Show first few forms
        (format t "~%First 5 forms:~%")
        (dotimes (i (min 5 (length forms)))
          (format t "  ~A. ~S~%" (1+ i) (nth i forms)))

        ; Test our extraction function specifically
        (format t "~%Testing extraction function:~%")
        (let ((defthm-count 0)
              (defun-count 0)
              (other-count 0))
          (dolist (form forms)
            (cond
              ((and (listp form) (equal (car form) 'defthm))
               (incf defthm-count)
               (format t "  Found DEFTHM: ~A~%" (second form)))
              ((and (listp form) (equal (car form) 'defun))
               (incf defun-count)
               (format t "  Found DEFUN: ~A~%" (second form)))
              (t (incf other-count))))

          (format t "  DEFTHM forms: ~A~%" defthm-count)
          (format t "  DEFUN forms: ~A~%" defun-count)
          (format t "  Other forms: ~A~%" other-count))))))

(analyze-forms)
(quit)