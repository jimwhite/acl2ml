(require 'cl)


(defvar lemmas nil)

(defun extract-names ()
  (do ((temp lemmas (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (setf temp2 (append temp2 (list (caar temp))))))


(defvar definitions nil)
(defvar defs-names nil)
(defvar defs-statem nil)

(defun extract-defs-names  ()
  (do ((temp definitions (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (setf temp2 (append temp2 (list (caar temp))))))

(defun show-definitions ()
  (insert (format "%s" definitions)))



(defvar lemma-dependencies nil)

(defvar lemma-names nil) 

(defvar tmp-file nil)

(defun read-tmp-file (tag)
  (let ((finished nil))
    (while (not finished)
      (setf tmp-file (read-lines (concat *home-dir* "tmp.out")) )
      (if (search tag tmp-file :from-end t)
	  (setf finished t)
	(sleep-for 3)))))






(defun lemma-statements (lemmas)
  (save-excursion 
  (do ((temp1 lemmas (cdr temp1))
       (temp2 nil))
      ((endp temp1) temp2)
    (if (member (car temp1) non-relevant-lemmas)
	nil
	(setf temp2 (append temp2 (list (list (car temp1) (concat (see-definition (car temp1)) ")"))))))
    )))


(defvar statem nil)


(defun remove-second (l)
  (list (nth 0 l) (nth 1 l) (cadddr l)))


(defun remove-second-and-third (l)
  (list (nth 0 l) (nth 1 l) (cadddr (cdr l))))

(defun extract-tables-recursive ()
  
  (setq lemmas nil)
  (setq definitions nil)
  (setq point (point))
  (beginning-of-buffer)
  (setf rep nil)
  
;; We remove the file tmp.out and the redirect ACL2 output to that file
  (if lemmaanalogy
  (progn (shell-command (concat "rm " (concat *home-dir* "/tmp.out")))
	 
  (comint-send-string *acl2-buffer-name* ":ubt! 1\n")
  (comint-send-string *acl2-buffer-name* (format "(mv-let (chan state)
        (open-output-channel \"%s\" :character state)
        (set-proofs-co chan state))" (concat *home-dir* "/tmp.out")))
  (comint-send-string *acl2-buffer-name* "(set-standard-co (proofs-co state) state)")))
  (let* ((buf (buffer-name))
	 (name (if (search "." buf) (subseq buf 0 (search "." buf)) buf)))
  (while (and (< (point) point) (not rep))
    (forward-sexp 1)
    (setf end (point))
    (backward-sexp 1)
    (setf beg (point))
    (setf a (read-from-string (buffer-substring beg end)))
    (forward-sexp 1)
    
    (cond ((equal (car (read-from-string (upcase (format "%s" (caar a))))) 'DEFTHM)
	   (let* ((info (build-table (extract-info (car a))))
		  (lemmaname (car info)))
	   (progn 
	     (if (member lemmaname  (extract-names))
		 (setq rep t)
	       (setq lemmas (append lemmas (list info))))
	     (save-excursion (eval-this-event))
	     (setq lemma-names (append lemma-names (list lemmaname)))
	     (setq statem (append statem (list (car a)))))))

	  ((equal (car (read-from-string (upcase (format "%s" (caar a))))) 'DEFUN)
	   (let* ((declaret (equal (car (nth 3 (car a))) 'declare))
		  (info (if declaret
			    (search-for-recursive-calls (build-table (extract-info (remove-second-and-third (car a)))))
			  (search-for-recursive-calls (build-table (extract-info (remove-second (car a)))))))
		  (definitionname (car info)))
	   (progn 
	     (if (member definitionname  (extract-defs-names))
		 (setq rep t)
	       (setq definitions (append definitions (list info))))
	    (save-excursion (eval-this-event))
	    (sit-for 1)
	    (if declaret 
		(add-guards-defun definitionname (nth 2 (car a)) (nth 4 (car a)) (nth 3 (car a)))
	      (add-guards-defun definitionname (nth 2 (car a)) (nth 3 (car a)) nil))
	    (comint-send-string *acl2-buffer-name* ":u \n")
	    (save-excursion (eval-this-event))
	    (setq defs-names (append defs-names (list definitionname)))
	    (setq defs-statem (append defs-statem (list (car a)))))))

	   
	  
	  (t (save-excursion (eval-this-event)  ))
	  )))
)





(defun extract-tables-next-event ()
  
  (setq point (point))
  (setf rep nil)
  
;; We remove the file tmp.out and the redirect ACL2 output to that file
  (let* ((buf (buffer-name))
	 (name (if (search "." buf) (subseq buf 0 (search "." buf)) buf)))
    (forward-sexp 1)
    (setf end (point))
    (backward-sexp 1)
    (setf beg (point))
    (setf a (read-from-string (buffer-substring beg end)))
    (forward-sexp 1)
    
    (cond ((equal (car (read-from-string (upcase (format "%s" (caar a))))) 'DEFTHM)
	   (let* ((info (build-table (extract-info (car a))))
		  (lemmaname (car info)))
	   (progn 
	     (if (member lemmaname  (extract-names))
		 (setq rep t)
	       (setq lemmas (append lemmas (list info))))
	     
	    ; (storing-lemmaanalogy-case lemmaname)
	     
	    (save-excursion (eval-this-event))
	     (setq lemma-names (append lemma-names (list lemmaname)))
	     (setq statem (append statem (list (car a)))))))

	  ((equal (car (read-from-string (upcase (format "%s" (caar a))))) 'DEFUN)
	   (let* ((declaret (equal (car (nth 3 (car a))) 'declare))
		  (info (if declaret
			    (search-for-recursive-calls (build-table (extract-info (remove-second-and-third (car a)))))
			  (search-for-recursive-calls (build-table (extract-info (remove-second (car a)))))))
		  (definitionname (car info)))
	   (progn 
	     (if (member definitionname  (extract-defs-names))
		 (setq rep t)
	       (setq definitions (append definitions (list info))))
	    (save-excursion (eval-this-event))
	    (sit-for 1)
	    (if declaret 
		(add-guards-defun definitionname (nth 2 (car a)) (nth 4 (car a)) (nth 3 (car a)))
	      (add-guards-defun definitionname (nth 2 (car a)) (nth 3 (car a)) nil))
	    (comint-send-string *acl2-buffer-name* ":u")
	    (save-excursion (eval-this-event))
	    (setq defs-names (append defs-names (list definitionname)))
	    (setq defs-statem (append defs-statem (list (car a)))))))
	  ((equal (car (read-from-string (upcase (format "%s" (caar a))))) 'INCLUDE-BOOK)
	   (progn 
	     (save-excursion (eval-this-event)  )
	     (if (equal (nth 3 (car a)) :system)
		 (setf included-libraries-system (append included-libraries-system (list (cadar a))))
	       (setf included-libraries (append included-libraries (list (cadar a)))))
	     (setf acl2guards (append acl2guards (ignore-errors (import-guards (cadar a)))))
	   ))

	  ((equal (car (read-from-string (upcase (format "%s" (caar a))))) 'DEFMACRO)
	   (progn (save-excursion (eval-this-event))
		  (sit-for 1)
		  (let ((info (build-table (extract-info (list 'defmacro (nth 1 (car a)) (obtain-expansion-term (cons (nth 1 (car a)) (remove-special (nth 2 (car a)))))))))
			(definitionname (car info)))
		    (if (member definitionname  (extract-defs-names))
			(setq rep t)
		      (setq definitions (append definitions (list info))))
		    (comint-send-string *acl2-buffer-name* ":u")
		    (save-excursion (eval-this-event))
		    (setq defs-names (append defs-names (list definitionname)))
		    (setq defs-statem (append defs-statem (list (car a)))))))
		
	  
	  (t (save-excursion (eval-this-event)  ))
	  ))
  )

(defun remove-special (l)
  (if (endp l)
      nil
    (if (string= "&" (subseq (symbol-name (car l)) 0 1))
	nil
      (cons (car l) (remove-special (cdr l))))))


(defvar included-libraries-system nil)
(defvar included-libraries nil)



(defun storing-lemmaanalogy-case (lemma)
  (shell-command (concat "rm " (concat *home-dir* "/tmp.out")))
  (shell-command (concat "touch " (concat *home-dir* "/tmp.out")))
  (comint-send-string *acl2-buffer-name* (format "(mv-let (chan state)
        (open-output-channel \"%s\" :character state)
        (set-proofs-co chan state))\n" (concat *home-dir* "/tmp.out")))
  (comint-send-string *acl2-buffer-name* "(set-standard-co (proofs-co state) state)\n")
  
  (save-excursion (eval-this-event))
  ;; We return the output to ACL2 shell
  (comint-send-string *acl2-buffer-name* "'finished\n")
  
  (generate-dependencies-lemma lemma)

  (comint-send-string *acl2-buffer-name* "'finished-deps\n")
  
  
  (comint-send-string *acl2-buffer-name* "(acl2::set-standard-co acl2::*standard-co* acl2::state)\n")
  (comint-send-string *acl2-buffer-name* "(acl2::set-proofs-co acl2::*standard-co* acl2::state)\n")
  (comint-send-string *acl2-buffer-name* "\n")
  (comint-send-string *acl2-buffer-name* ":u\n")
  (save-excursion (eval-this-event))
  )



(defun extract-recursive-beg-to-end ()
  
  (end-of-buffer)
  (extract-tables-recursive)
;  (generate-dependencies)
  )



(defun extract-recursive-beg-to-end2 ()
  
  (with-current-buffer (current-buffer)
    (end-of-buffer)
    (backward-sexp 1)
    (forward-sexp 1)
    (setf end0 (point))
    (let ((b (beginning-of-buffer)))    
      (while (< (point) end0)
	(ignore-errors(extract-tables-next-event))
	))
    ))




;(defun extract-tables-upto-here ()
;  
 ; (let ((end1 (point)))
  ;  (with-current-buffer (current-buffer)
;   (let ((b (beginning-of-buffer))
;	    (p (point)))
 ;     (while (< p end1)
;	(extract-tables-next-event)
;	(setf p (point)))))))

(defvar firsttry t)

(defun extract-tables-upto-here ()
  
  (save-excursion
    (backward-sexp 1)
    (forward-sexp 1)
    (let ((final (point)))
      (beginning-of-buffer)
      (extract-tables-next-event)
      (while (< (point) final)
	(if (looking-at "#")
	    (forward-char 1))
	(ignore-errors (extract-tables-next-event))))
    )
  )



(defvar lemma-vectors nil)

(defun convert-recursive ()
  (do ((temp lemmas (cdr temp)))
      ((endp temp) nil)
    (setf lemma-vectors (append lemma-vectors (list (flatten-table (populate-table  (car temp))))))))


(defvar lemmas-libraries nil)


(defun convert-recursive-several-libraries ()
  (setf lemma-vectors nil)
  (do ((temp lemmas-libraries (cdr temp)))
      ((endp temp) nil)
    (setf lemma-vectors (append lemma-vectors (list (flatten-table (populate-table  (car temp))))))))


(defvar defs-vectors nil)

(defun convert-recursive-several-libraries-defs ()
  (setf defs-vectors nil)
  (do ((temp definitions-libraries (cdr temp)))
      ((endp temp) nil)
    (setf defs-vectors (append defs-vectors (list (flatten-table (populate-table  (car temp))))))))



(defvar definition-libraries nil)

    



;;; Generation of lemma dependencies


(defun generate-dependencies ()
  
  (setf lemma-dependencies nil)
  (save-excursion (read-tmp-file "FINISHED")
  (let* ((buf (buffer-name))
	 (name (if (search "." buf) (subseq buf 0 (search "." buf)) buf)))
    (lemma-statements-gen lemma-names)
    (do ((temp0 lemma-names (cdr temp0)))
	((endp temp0) nil)
      (lemma-statements-gen (what-rewrite-rules-are-used-in (car temp0))))
    (comint-send-string *acl2-buffer-name* (format "'FINISHED-THM \n"))
    (read-tmp-file "FINISHED-THM")
    (do ((temp lemma-names (cdr temp)))
	((endp temp) (modify-lemma-dependencies))
      (setq lemma-dependencies (append lemma-dependencies (list (append (list (read-from-string (format "%s" (car temp))) )
									(list (concat (see-definition (upcase (format "%s" (car temp)))) ")")
									(lemma-statements 
									 (what-rewrite-rules-are-used-in (car temp))
									 )
									)))))
      ))))


(defun generate-dependencies-lemma (lemma)
  
  (save-excursion (read-tmp-file "FINISHED")
  (let* ((buf (buffer-name))
	 (name (if (search "." buf) (subseq buf 0 (search "." buf)) buf)))
    (lemma-statements-gen (list lemma))
    (lemma-statements-gen (what-rewrite-rules-are-used-in lemma))
    (comint-send-string *acl2-buffer-name* (format "'FINISHED-THM \n"))
    (read-tmp-file "FINISHED-THM")
    (setq lemma-dependencies (append lemma-dependencies (list (append (list (car (read-from-string (format "%s" lemma)) ))
									(list (list (concat (see-definition (upcase (format "%s" lemma))) ")")
									(lemma-statements 
									 (what-rewrite-rules-are-used-in lemma)
									 ))
									)))))
    
    )))


(defun see-lemma-dependencies ()
  
  (insert (format "%s" lemma-dependencies)))

(defun see-lemma-names ()
  
  (insert (format "%s" lemma-names)))


(defun modify-lemma-dependencies ()
  (do ((temp lemma-dependencies (cdr temp))
       (temp2 nil))
      ((endp temp) (setf lemma-dependencies temp2))
    (setf temp2 (append temp2 (list (list (caaar temp) (cdar temp)))))))



(defun lemma-statements-gen (lemmas)
  (save-excursion (do ((temp lemmas (cdr temp)))
		      ((endp temp) nil)
		    (comint-send-string *acl2-buffer-name* (format ":pe %s \n 'end-thm \n" (car temp))))))



(defvar non-relevant-lemmas 
  '("CAR-CONS" "CDR-CONS" "ACL2::CAR-CONS" "ACL2::CDR-CONS" "ACL2::|(* (* x y) z)|" "|(* (* x y) z)|" "ACL2::REMOVE-WEAK-INEQUALITIES"
    "REMOVE-WEAK-INEQUALITIES" "ACL2::|(* 1 x)|" "|(* 1 x)|" "ACL2::|(* y x)|" "|(* y x)|" "ACL2::|(+ y x)|" "|(+ y x)|"
    "ACL2::|(* -1 x)|" "ACL2::|(* 1 x)|" "ACL2::|(* c (* d x))|" "ACL2::|(* x (+ y z))|" "ACL2::|(* x (- y))|" "ACL2::|(* x x)|" "ACL2::|(* y (* x z))|" "ACL2::|(* y x)|" "ACL2::|(+ (+ x y) z)|" "ACL2::|(+ (- x) (* c x))|" "ACL2::|(+ 0 x)|" "ACL2::|(+ c (+ d x))|" "ACL2::|(+ x (* c x))|" "ACL2::|(+ y (+ x z))|" "ACL2::|(+ y x)|" "ACL2::|(- (* c x))|" "ACL2::|(expt (+ x y) 2)|" "ACL2::BUBBLE-DOWN-*-MATCH-1" "ACL2::BUBBLE-DOWN-+-MATCH-3" "ACL2::NORMALIZE-ADDENDS" "ACL2::NORMALIZE-FACTORS-GATHER-EXPONENTS" "ACL2::REMOVE-WEAK-INEQUALITIES" 
"ACL2::|(* 1 x)|" "ACL2::|(* x (+ y z))|" "ACL2::|(* x x)|" "ACL2::|(* y x)|" "ACL2::|(+ 0 x)|" "ACL2::|(+ y x)|" "ACL2::BUBBLE-DOWN-*-MATCH-1"
      "ACL2::NORMALIZE-FACTORS-GATHER-EXPONENTS" "ACL2::|(* 0 x)|" "|(* 0 x)|"
))




