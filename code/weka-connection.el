;;; Weka invokation

(defun last-part-of-lists (list)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
      (setf temp2 (append temp2 (list (cadar temp))))))


(defun print-list (list)
  (do ((temp list (cdr temp))
       (temp2 ""))
      ((endp temp) (subseq temp2 0 (1- (length temp2))))
    (setf temp2 (concat temp2 (format "%s," (car temp))) )))

(defun convert-all-lemmas-to-weka-format ()
  (convert-recursive)
  (do ((temp (last-part-of-lists lemma-vectors) (cdr temp))
	 (temp2 ""))
	((endp temp) temp2)
	(setf temp2 (concat temp2 (format "%s\n"  (print-list  (car temp) ))))))


(defun convert-all-lemmas-to-weka-format-several ()
  (add-several-libraries)
  (convert-recursive-several-libraries)
  (do ((temp (last-part-of-lists lemma-vectors) (cdr temp))
	 (temp2 ""))
	((endp temp) temp2)
	(setf temp2 (concat temp2 (format "%s\n"  (print-list  (car temp) ))))))




(defun weka ()
  (let ((alg (cond ((string= "k" algorithm) "SimpleKMeans")
		   ((string= "e" algorithm) "EM")
		   ((string= "f" algorithm) "FarthestFirst")))
	(n 0))
    (shell-command (concat "rm" (expand-file-name "temp.csv")))
    (with-temp-file (expand-file-name "temp.csv") (insert (convert-all-lemmas-to-weka-format-several)))
    (setf n (cond  ((eq 2 granularity-level) (floor (length lemmas-libraries) 7))
		  ((eq 3 granularity-level) (floor (length lemmas-libraries) 5))
		  ((eq 4 granularity-level) (floor (length lemmas-libraries) 4))
		  ((eq 5 granularity-level) (floor (length lemmas-libraries) 2))
		  (t (floor (length lemmas-libraries) 8))))
    (shell-command  (concat "sleep 1; cat " *home-dir* "aux_files/headers.txt " 
			    (expand-file-name "temp.csv") " > " 
			    (expand-file-name "temp3.arff")))
    (shell-command (concat "java -classpath " 
			   *weka-dir*
			   " weka.filters.unsupervised.attribute.AddCluster -W \"weka.clusterers." alg " -N " (format "%s" n) " -S 42\" -I last -i "
			 (expand-file-name "temp3.arff") " -o " (expand-file-name "out.arff")))
    (shell-command (concat "tail -n +56 "
			   (expand-file-name "out.arff") " > " (expand-file-name "out_bis.arff")   ))

    (if whysimilar
	(shell-command (concat "java -classpath " 
			 *weka-dir*
			 " weka.attributeSelection.InfoGainAttributeEval -s \"weka.attributeSelection.Ranker -T 0 -N 5\" -i "
			 (expand-file-name "out.arff") " > " (expand-file-name "whysimilar.txt")))
	)
    ))




(defun convert-all-definitions-to-weka-format-several ()
  (add-several-libraries-defs)
  (setf definitions-libraries (remove-nil2 definitions-libraries))
  (convert-recursive-several-libraries-defs)
  (do ((temp (last-part-of-lists defs-vectors) (cdr temp))
	 (temp2 ""))
	((endp temp) temp2)
	(setf temp2 (concat temp2 (format "%s\n"  (print-list  (car temp) ))))))


(defun weka-defs ()
  (let ((alg (cond ((string= "k" algorithm) "SimpleKMeans")
		   ((string= "e" algorithm) "EM")
		   ((string= "f" algorithm) "FarthestFirst")))
	(n 0))
    (shell-command (concat "rm" (expand-file-name "temp.csv")))
    (with-temp-file (expand-file-name "temp.csv") (insert (convert-all-definitions-to-weka-format-several)))
    (setf n (cond  ((eq 2 granularity-level) (floor (length definitions-libraries) 7))
		  ((eq 3 granularity-level) (floor (length definitions-libraries) 5))
		  ((eq 4 granularity-level) (floor (length definitions-libraries) 4))
		  ((eq 5 granularity-level) (floor (length definitions-libraries) 2))
		  (t (floor (length definitions-libraries) 8))))
    
    (shell-command  (concat "sleep 1; cat " *home-dir* "aux_files/headers.txt " 
			    (expand-file-name "temp.csv") " > " 
			    (expand-file-name "temp3.arff")))
    (shell-command (concat "sleep 1; java -classpath " 
			   *weka-dir*
			   " weka.filters.unsupervised.attribute.AddCluster -W \"weka.clusterers." alg " -N " (format "%s" n) " -S 42\" -I last -i "
			 (expand-file-name "temp3.arff") " -o " (expand-file-name "out.arff")))
    (shell-command (concat "tail -n +56 "
			   (expand-file-name "out.arff") " > " (expand-file-name "out_bis.arff")   ))

    (if whysimilar
	(shell-command (concat "java -classpath " 
			 *weka-dir*
			 " weka.attributeSelection.InfoGainAttributeEval -s \"weka.attributeSelection.Ranker -T 0 -N 5\" -i "
			 (expand-file-name "out.arff") " > " (expand-file-name "whysimilar.txt")))
	)
    ))


;;; Explain the reason for similarities


(defun why-are-similar ()
  (sleep-for 2)
  (let* ((file (read-lines (expand-file-name "whysimilar.txt")) )
	 (attributes (subseq file (+ 21 (search "Selected attributes:" file)))))
    (extract-selected-attributes (subseq attributes 0 (1- (search ":" attributes))) nil)))


  

(defun extract-selected-attributes (temp res)
 (let ((comma (search "," temp)))
   (if comma
       (extract-selected-attributes (subseq temp (+ 1 comma)) 
				    (append res (list (car (read-from-string (subseq temp 0 comma))))))
     (append res (list (car (read-from-string temp)))))))






(defun explain-why-are-similar ()
  (let ((sim (why-are-similar)))
    (insert (format "The similarities of these lemmas are given by the following parameters:\n"))
    (do ((temp sim (cdr temp)))
	((endp temp) (insert (format "------------------------------------------------------------------------------------------------\n")))
      (insert (format " - %s\n" (attribute-to-value (car temp)))))))
      

(defun attribute-to-value (n)
  (let* ((tdl (cond ((< n 8) 1)
		   ((< n 15) 2)
		   ((< n 22) 3)
		   ((< n 29) 4)
		   ((< n 36) 5)
		   ((< n 43) 6)
		   (t 7)))
	(arity (- (- n (* 7 (- tdl 1))) 1)))
    (if (= arity 0)
	(format "The variables of the term-tree at depth level %s" tdl)
    (format "The function(s) of arity %s of the term-tree at depth level %s"  (1- arity) tdl))))
    











;;; Printing clusters


(defun 0_n (n)
  (do ((i 0 (1+ i))
       (temp nil))
      ((= i n) temp)
      (setf temp (append temp (list (list i nil))))))


(defun read-lines1 (file)
  "Return a list of lines in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string
     (buffer-string) "\n" t)
    ))


(defun lines-to-clusters (lines)
  (do ((temp lines (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
      (setf temp2 (append temp2 (list (string-to-number (subseq (car temp) (+ 7 (search "cluster" (car temp) :from-end t)))))))
      ))



(defun extract-clusters-from-file ()
  (let* ((lines (read-lines1 (expand-file-name "out_bis.arff"))))
    (lines-to-clusters lines)))


(defun form-clusters (list n)
  (do ((i 0 (1+ i))
       (temp nil))
      ((= i n) temp)
      (setf temp (append temp (list (clusters-of-n list i))))))
 






(defun clusters-of-n (list n)
  (do ((temp list (cdr temp))
       (i 1 (1+ i))
       (temp2 nil))
      ((endp temp) temp2)
      (if (equal (car temp) n)
	  (setf temp2 (append temp2 (list i))))))
       


(defun remove-alone (list)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
      (if (> (length (car temp)) 1) ;(or (not (= (length (car temp)) 1))  (not (= (length (car temp)) 0)))
	  (setf temp2 (append temp2 (list (car temp)))))))




(defun print-clusters-weka (gra)
  (let* ((clusters (extract-clusters-from-file ))
	 (res1 (remove-alone (cdr (form-clusters clusters gra)))))
    (with-current-buffer "*display*"
      (erase-buffer)
      (insert (format "We have found the following clusters:\n" ))
      (insert (format "------------------------------------------------------------------------------------------------\n" ))
  
      (do ((temp res1 (cdr temp))
	   (i 1 (1+ i)))
	  ((endp temp) (insert (format "------------------------------------------------------------------------------------------------\n")) )
	  (progn (insert (format "Cluster %s\n" i )) 
		 (do ((temp2 (car temp) (cdr temp2)))
		     ((endp temp2) (insert (format "\n")))
		     (progn (insert (format "Lemma %s\n" (car (nth (1- (car temp2)) lemmas-libraries))))
			    
				)
		     )
		 (insert (format "\n"))))
      (if whysimilar (explain-why-are-similar))
      
      )))


(defun print-clusters-weka-defs (gra)
  (let* ((clusters (extract-clusters-from-file ))
	 (res1 (remove-alone (cdr (form-clusters clusters gra)))))
    (with-current-buffer "*display*"
      (erase-buffer)
      (insert (format "We have found the following clusters:\n" ))
      (insert (format "------------------------------------------------------------------------------------------------\n" ))
  
      (do ((temp res1 (cdr temp))
	   (i 1 (1+ i)))
	  ((endp temp) (insert (format "------------------------------------------------------------------------------------------------\n")) )
	  (progn (insert (format "Cluster %s\n" i )) 
		 (do ((temp2 (car temp) (cdr temp2)))
		     ((endp temp2) (insert (format "\n")))
		     (progn (insert (format "Definition %s\n" (car (nth (1- (car temp2)) definitions-libraries))))
			    
				)
		     )
		 (insert (format "\n"))))
      (if whysimilar (explain-why-are-similar))
      
      )))



;;; Similarities for theorems 

(defvar similarlemmas nil)

(defun extract-deps-similar-lemmas ()
  
  (do ((temp similarlemmas (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (setf temp2 (append temp2 (list (assoc (car temp) lemma-dependencies))))))

(defun showsimilarlemmas ()
  
  (insert (format "%s" similarlemmas)))

(defun print-similarities-weka ()
  (let* ((clusters (extract-clusters-from-file ))
	 (temp1 (clusters-of-n clusters (nth (1- (length lemmas)) clusters))))
    (progn 
    (with-current-buffer "*display*"
      (erase-buffer)
      (setf similarlemmas nil)
      (if (or (not temp1) (equal (length temp1) 1))
	  (insert (format "Sorry no similarities"))
      (progn 
	     (insert (format "Similarities:\n"))
      (insert (format "------------------------------------------------------------------------------------------------\n"))
      (if (equal (length temp1) 2)
	  (insert (format "Theorem %s is similar to theorem:\n" (car (last lemma-names))))
	(insert (format "Theorem %s is similar to theorems:\n" (car (last lemma-names)))))
      (do ((temp2 temp1 (cdr temp2)))
	  ((endp temp2) )
	  (if (not (string= (format "%s" (car (nth (- (car temp2)  1) lemmas-libraries)) )
			    (format "%s" (caar (last lemmas)))))
	      (progn 
(setq similarlemmas (append similarlemmas (list (car (nth (- (car temp2)  1) lemmas-libraries)))))
		(insert (format "- %s\n" (car (nth (- (car temp2)  1) lemmas-libraries)))))
	  ))
    (if lemmaanalogy  (format-to-lemma-analogy (car (nth (1- (length lemmas)) lemma-dependencies))
			     (nth (1- (length lemmas)) statem)
(extract-deps-similar-lemmas)))
      (insert (format "------------------------------------------------------------------------------------------------\n"))
(if whysimilar (explain-why-are-similar))

(if lemmaanalogy  (invoke-lemma-analogy))
))))))


(defun print-similarities-weka1 (lemma)
  (let* ((clusters (extract-clusters-from-file ))
	 (temp1 (clusters-of-n clusters (nth (position lemma lemma-names) clusters))))
    (progn 
    (with-current-buffer "*display*"
      (erase-buffer)
      (setf similarlemmas nil)
      (if (or (not temp1) (equal (length temp1) 1))
	  (insert (format "Sorry no similarities"))
      (progn 
	     (insert (format "Similarities:\n"))
      (insert (format "------------------------------------------------------------------------------------------------\n"))
      (if (equal (length temp1) 2)
	  (insert (format "Theorem %s is similar to theorem:\n" lemma))
	(insert (format "Theorem %s is similar to theorems:\n" lemma)))
      (do ((temp2 temp1 (cdr temp2)))
	  ((endp temp2) )
	  (if (not (string= (format "%s" (car (nth (- (car temp2)  1) lemmas-libraries)) )
			    lemma))
	      (progn 
		(setq similarlemmas (append similarlemmas (list (car (nth (- (car temp2)  1) lemmas-libraries)))))
		(insert (format "- %s\n" (car (nth (- (car temp2)  1) lemmas-libraries)))))
	  ))
    (if lemmaanalogy  (format-to-lemma-analogy (car (nth (1- (length lemmas)) lemma-dependencies))
			     (nth (1- (length lemmas)) statem)
(extract-deps-similar-lemmas)))
      (insert (format "------------------------------------------------------------------------------------------------\n"))
(if whysimilar (explain-why-are-similar))

(if lemmaanalogy  (invoke-lemma-analogy))
))))))



(defun print-similarities-weka-defs ()
  (let* ((clusters (extract-clusters-from-file ))
	 (temp1 (clusters-of-n clusters (nth (1- (length definitions)) clusters))))
    (progn 
    (with-current-buffer "*display*"
      (erase-buffer)
      
      (if (or (not temp1) (equal (length temp1) 1))
	  (insert (format "Sorry no similarities"))
      (progn 
	     (insert (format "Similarities:\n"))
      (insert (format "------------------------------------------------------------------------------------------------\n"))
      (if (equal (length temp1) 2)
	  (insert (format "Definition %s is similar to definition:\n" (car (last defs-names))))
	(insert (format "Definition %s is similar to definitions:\n" (car (last defs-names)))))
      (do ((temp2 temp1 (cdr temp2)))
	  ((endp temp2) )
	  (if (not (string= (format "%s" (car (nth (- (car temp2)  1) definitions-libraries)) )
			    (format "%s" (caar (last definitions)))))
	      (progn 
		(insert (format "- %s\n" (car (nth (- (car temp2)  1) definitions-libraries)))))
	  ))
      (insert (format "------------------------------------------------------------------------------------------------\n"))
(if whysimilar (explain-why-are-similar))
))))))


(defun print-similarities-weka-defs1 (def)
  (let* ((clusters (extract-clusters-from-file ))
	 (temp1 (clusters-of-n clusters (nth (position def defs-names) clusters))))
    (progn 
    (with-current-buffer "*display*"
      (erase-buffer)
      
      (if (or (not temp1) (equal (length temp1) 1))
	  (insert (format "Sorry no similarities"))
      (progn 
	     (insert (format "Similarities:\n"))
      (insert (format "------------------------------------------------------------------------------------------------\n"))
      (if (equal (length temp1) 2)
	  (insert (format "Definition %s is similar to definition:\n" def))
	(insert (format "Definition %s is similar to definitions:\n" def)))
      (do ((temp2 temp1 (cdr temp2)))
	  ((endp temp2) )
	  (if (not (string= (format "%s" (car (nth (- (car temp2)  1) definitions-libraries)) )
			    def))
	      (progn 
		(insert (format "- %s\n" (car (nth (- (car temp2)  1) definitions-libraries)))))
	  ))
      (insert (format "------------------------------------------------------------------------------------------------\n"))
(if whysimilar (explain-why-are-similar))
))))))


;;; Alltogether


(defun clusters ()
  
;  (extract-tables-recursive)
  (switch-to-buffer-other-window "*display*")
  (weka)
  (sleep-for 2)
  (print-clusters-weka (cond  ((eq 2 granularity-level) (floor (length lemmas-libraries) 7))
			      ((eq 3 granularity-level) (floor (length lemmas-libraries) 5))
			      ((eq 4 granularity-level) (floor (length lemmas-libraries) 4))
			      ((eq 5 granularity-level) (floor (length lemmas-libraries) 2))
			      (t (floor (length lemmas-libraries) 8))))
  )



(defun cluster-definitions ()
  
 ; (extract-tables-recursive)
  (switch-to-buffer-other-window "*display*")
  (weka-defs)
  (sleep-for 2)
  (print-clusters-weka-defs (cond  ((eq 2 granularity-level) (floor (length definitions-libraries) 7))
			      ((eq 3 granularity-level) (floor (length definitions-libraries) 5))
			      ((eq 4 granularity-level) (floor (length definitions-libraries) 4))
			      ((eq 5 granularity-level) (floor (length definitions-libraries) 2))
			      (t (floor (length definitions-libraries) 8))))
  )


(defun cluster-general ()
  
  (let ((res (read-string "Cluster definitions or theorems (definitions -> d, theorems -> t): ")))
    (cond ((string= res "d")
	   (progn 
	     (setf option-libs-defs  (read-string "Cluster definitions using current library (c), libraries that you have exported (m), libraries that you have selected in the menu (s), libraries that you have loaded (l), or in the whole library (g):"))
	     (cond ((member option-libs-defs '("c" "m" "s" "l" "g"))
		   (progn (switch-to-buffer-other-window "*display*")
			  (weka-defs)
			  (sleep-for 2)
			  (print-clusters-weka-defs (cond  ((eq 2 granularity-level) (floor (length definitions-libraries) 7))
							   ((eq 3 granularity-level) (floor (length definitions-libraries) 5))
							   ((eq 4 granularity-level) (floor (length definitions-libraries) 4))
							   ((eq 5 granularity-level) (floor (length definitions-libraries) 2))
							   (t (floor (length definitions-libraries) 8))))))
		  (t (format "That option is not valid")))
	     ))
	  ((string= res "t")
	   (progn 
	     (switch-to-buffer-other-window "*display*")
	     (weka)
	     (sleep-for 2)
	     (print-clusters-weka (cond  ((eq 2 granularity-level) (floor (length lemmas-libraries) 7))
					 ((eq 3 granularity-level) (floor (length lemmas-libraries) 5))
					 ((eq 4 granularity-level) (floor (length lemmas-libraries) 4))
					 ((eq 5 granularity-level) (floor (length lemmas-libraries) 2))
					 (t (floor (length lemmas-libraries) 8))))
	     ))
	  (t (format "Non-valid option")))))
  




(defun last-event ()
  
  (let ((beg nil)
	(end nil)
	(a nil))
    (backward-sexp 1)
    (setf beg (point))
    (forward-sexp 1)
    (setf end (point))
    (setf a (read-from-string (buffer-substring beg end)))
    ))



(defun show-similarities ()
  
 ; (extract-tables-recursive)
  (switch-to-buffer-other-window "*display*")
  (weka)
  (sleep-for 2)
  (print-similarities-weka))



(defun show-similarities-defs ()
  
 ; (extract-tables-recursive)
  (switch-to-buffer-other-window "*display*")
  (weka-defs)
  (sleep-for 2)
  (print-similarities-weka-defs))


(defun show-similarities-general ()
  
   (forward-sexp 1)
   (setf end (point))
   (backward-sexp 1)
   (setf beg (point))
   (setf a (read-from-string (buffer-substring beg end)))
   (cond ((equal (car (read-from-string (upcase (format "%s" (caar a))))) 'DEFTHM)
	   (progn 
	     (switch-to-buffer-other-window "*display*")
	     (weka)
	     (sleep-for 2)
	     (print-similarities-weka1 (nth 1 (car a))))
	  )
	 ((equal (car (read-from-string (upcase (format "%s" (caar a))))) 'DEFUN)
	  (progn 
	    (setf option-libs-defs  (read-string "Cluster definitions using current library (c), libraries that you have exported (m), libraries that you have selected in the menu (s), libraries that you have loaded (l), or in the whole library (g):"))
	    (cond ((member option-libs-defs '("c" "m" "s" "l" "g"))
		   (progn (switch-to-buffer-other-window "*display*")
			  (weka-defs)
			  (sleep-for 2)
			  (print-similarities-weka-defs1 (nth 1 (car a)))))
		  (t (format "That option is not valid")))
	  ))
	  (t (format "This only works for theorems and definitions"))
	  ))
  

  










  

