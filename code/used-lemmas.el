(defun eval-this-event ()
  "Send the current ACL2 expression to the ACL2 process using proper ACL2 evaluation"
  (save-excursion
    (let ((start (progn (backward-sexp 1) (point)))
          (end (progn (forward-sexp 1) (point))))
      (if (and (boundp 'inferior-acl2-buffer) 
               (get-buffer inferior-acl2-buffer)
               (fboundp 'send-region-to-acl2-process))
          (send-region-to-acl2-process start end nil)
        (if (and (boundp '*acl2-buffer-name*) 
                 (get-buffer *acl2-buffer-name*))
            (let ((expression (buffer-substring start end)))
              (comint-send-string *acl2-buffer-name* (concat expression "\n")))
          (message "ACL2 buffer not available"))))))


(defun read-lines (file)
  "Return a list of lines in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)
    ))


(defun what-rewrite-rules-are-used-in (name)
  
  (let* ((thm (search (format "( DEFTHM %s" (upcase (format "%s" name))) tmp-file))
	 (rules-s (search "Rules: " tmp-file :start2 thm))
	 (time (search "Time: " tmp-file :start2 thm))
	 (rules (subseq tmp-file rules-s time)))
    (rewrite-runes rules)))



(defun rewrite-runes (text)
  (do ((temp text) (temp2 nil) (end nil))
      (end temp2)
    (let ((rew (search ":REWRITE" temp)))
      (if rew 
	  (progn (setf temp2 (append temp2 (list (remove-last-parenthesis (subseq (subseq temp (+ rew 9) (search "\n" temp :start2 rew)) 0
							 (1- (length (subseq temp (+ rew 9) (search "\n" temp :start2 rew)))))))))
		 (setf temp (subseq temp (+ 1 rew))))
	(setf end t)))))



(defun remove-last-parenthesis (text)
  (let* ((temp text)
	 (par (string= (subseq temp (1- (length temp))) ")")))
    (while par
      (progn (setf temp (subseq temp 0 (1- (length temp))))
	     (setf par (string= (subseq temp (1- (length temp))) ")"))))
    temp))



(defun see-definition (name)
  
  (let* ((op1 (search (format "(DEFTHM %s" (format "%s" name)) tmp-file :from-end t))
	 (op2 (search (format "(DEFTHM\n               %s" (format "%s" name)) 
		      tmp-file :from-end t)))
    (if op1
	(let* ((start-thm op1)
	       (end-thm (search "END-THM" tmp-file :start2 start-thm)))
	  (subseq (subseq tmp-file start-thm end-thm) 0 (search ")" (subseq tmp-file start-thm end-thm) :from-end t) ))
      (if op2 
	  (let* ((start-thm op2)
	       (end-thm (search "END-THM" tmp-file :start2 start-thm)))
	  (subseq (subseq tmp-file start-thm end-thm) 0 (search ")" (subseq tmp-file start-thm end-thm) :from-end t) ))
	nil))))





(defun see-definition2 (name)
  (let ((file (buffer-name)))
    (switch-to-buffer *acl2-buffer-name*)
    (end-of-buffer)
    (condition-case nil
	(progn (search-backward (format "(DEFTHM %s" (upcase (format "%s" name))))
	       (mark-sexp 1)
	       (copy-to-register 'a (point-marker) (mark-marker))
	       (end-of-buffer)
	       (switch-to-buffer file)
	       (insert "(setq new-thm '")
	       (insert-register 'a)
	       (forward-sexp 1)
	       (insert ")")
	       (eval-last-sexp nil)
	       (backward-sexp 1)
	       (kill-sexp 1)
	       new-thm)
    (error (progn (sleep-for 2)
		  (see-definition2 name))))    
    
 
   
    ;(insert (format ";;; Usa: %s\n" (car lista-usados)))
    ;(mapcar (lambda (x) (insert (format ";;;      %s\n" x)))
	 ;   used-list)
    
    )

  )











