:q
(defun read-acl2-file (filename)
  (with-open-file (stream filename :direction :input)
    (let ((forms nil))
      (handler-case
          (loop for form = (read stream nil :eof)
                until (eq form :eof)
                do (push form forms))
        (error (e)
          (format t "Error reading ~A: ~A~%" filename e)))
      (reverse forms))))

(let ((forms (read-acl2-file "/home/acl2/books/arithmetic-3/bind-free/basic.lisp")))
  (format t "Successfully read ~A forms~%" (length forms))
  (when forms
    (format t "First form: ~S~%" (first forms))))

(quit)