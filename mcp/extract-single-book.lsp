:q
(defun read-and-process-acl2-file (filename output-filename)
  "Read ACL2 file and write forms to output for Common Lisp processing"
  (with-open-file (input filename :direction :input)
    (with-open-file (output output-filename :direction :output :if-exists :supersede)
      (let ((forms nil)
            (count 0))
        (handler-case
            (loop for form = (read input nil :eof)
                  until (eq form :eof)
                  do (progn
                       (push form forms)
                       (incf count)))
          (error (e)
            (format t "Error reading ~A: ~A~%" filename e)
            (return-from read-and-process-acl2-file nil)))

        ;; Write forms in simple format
        (format output "(~%")
        (dolist (form (reverse forms))
          (prin1 form output)
          (terpri output))
        (format output ")~%")

        (format t "Successfully processed ~A forms from ~A~%" count filename)
        count))))

;; Process the specific file
(read-and-process-acl2-file "/home/acl2/books/arithmetic-3/bind-free/basic.lisp"
                            "/tmp/basic-forms.lsp")

(quit)