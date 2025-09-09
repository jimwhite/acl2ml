;;;**************************************************
;;; Extraction of guards from functions
;;;**************************************************

;;;--------------------------------------------------
;; Notes for extraction of guards
;;
;; 1. First extract the definition of the function
;; 2. Extract the body of the function
;; 3. Expand all the macros of that function using :trans
;;    - Example :trans! (if (zp n) a (helper-fact (1- n) (* n a)))
;; 4. Extract all the functions used in the definition
;; 5. Obtain the guards of each definition using :args 
;; 6. Store the guards for further use
;; 
;; This process could be done every time a function is defined. 
;;;--------------------------------------------------

(require 'cl)

(defun init-testing ()
  (comint-send-string *acl2-buffer-name* "(include-book \"countereg-gen/top\" :dir :system :ttags :all)\n"))


(defun nested-lists-to-string (l)
  (do ((temp l (cdr temp))
       (res "("))
      ((endp temp) (concat res ")"))
    (if (listp (car temp))
	(setf res (concat res " " (nested-lists-to-string (car temp))))
      (setf res (concat res " " (format "%s" (car temp)))))))


(defun my-clean-acl2 ()
  (let ((this-buffer (buffer-name)))
    (switch-to-buffer *acl2-buffer-name*)
    (beginning-of-buffer)
    (search-forward-regexp "[^>]*>")
    (let ((inicio (point)))
      (end-of-buffer)
      (kill-region inicio (point))
      (switch-to-buffer this-buffer))))




;;;; Guards of functions

(defun read-guards () 
  (with-current-buffer *acl2-buffer-name*
    (beginning-of-buffer)
    (let ((beg1 (search-forward "Formals:         " nil t))
	  (s1 (forward-sexp 1))
	  (end1 (point))
	  (beg2 (search-forward "Guard:           " nil t))
	  (s2 (forward-sexp 1))
	  (end2 (point)))
      (list (car (read-from-string (buffer-substring beg1 end1)))
	    (car (read-from-string (buffer-substring beg2 end2)))))))  

(defun obtain-guards-fun-acl2 (fun)
  (progn 
    (my-clean-acl2)
    (comint-send-string *acl2-buffer-name* (format ":args %s\n" fun))
    (sit-for 0.1)
    (read-guards)))


(defun add-to-acl2guards (fun g)
  (setf acl2guards (append acl2guards (list (cons fun g)))))

(defun obtain-guards-function (fun)
  (if (assoc fun acl2guards)
      (cdr (assoc fun acl2guards))
    (progn (add-to-acl2guards fun (obtain-guards-fun-acl2 fun))
	   (cdr (assoc fun acl2guards)))))
      
	
(defun find-functions (term)
  (do ((temp term (cdr temp))
       (res (list (list (car term) (cdr term)))))
      ((endp temp) (remove-duplicates res :test #'equal))
      (if (listp (car temp))
	  (setf res (append (append res
			    (list (list (caar temp) (cdar temp))))
			    (find-functions (car temp))))
	))) 




(defun replace-occurrences (term x y)
  (if (endp term)
      nil
    (if (listp (car term))
	(cons (replace-occurrences (car term) x y)
	      (replace-occurrences (cdr term) x y))
      (if (equal (car term) x)
	  (cons y (replace-occurrences (cdr term) x y))
	(cons (car term) (replace-occurrences (cdr term) x y))))))


; (mapcar 'cadr (find-functions '(equal (helper-fact n a) (* (fact n) a))))


(defun replace-occurrences-list (term lx ly)
  (cond ((not (listp term)) 't)
	((equal (car term) 'quote))
	(t  (do ((temp lx (cdr temp))
		 (temp2 ly (cdr temp2))
		 (res term))
		((endp temp) res)
	      (setf res (replace-occurrences res (car temp) (car temp2)))))))


;(caddr (car acl2guards))

(defun remove-ands (l)
  (do ((temp l (cdr temp))
       (res nil))
      ((endp temp) (remove-duplicates res :test #'equal))
    (if (listp (car temp))
	(if (or (equal 'AND (caar temp))
		(equal 'and (caar temp)))
	    (setf res (append res (cdar temp)))
	  (setf res (append res (list (car temp)))))
      (if (or (equal (car temp) 't) (equal (car temp) 'T))
	  nil
	(setf res (append res (list (car temp))))))))

(defun obtain-guards (term)
  (let* ((ff (find-functions term)))
    (cons 'and  (remove-duplicates 
		(mapcar (lambda (x) (let* ((g (obtain-guards-function (car x)))
					   (args (car g))
					   (gu (cadr g)))
				      (replace-occurrences-list gu args (cadr x))
				      )) 
		       ff)
		:test #'equal))))
    
; (setf term '(EQUAL (HELPER-FACT N A) (BINARY-* (BINARY-+ '1 N) A)))
; (obtain-guards term)


;; Expansion of macros in terms



(defun read-expansion () 
  (with-current-buffer *acl2-buffer-name*
    (beginning-of-buffer)
    (let ((beg1 (search-forward ">" nil t))
	  (s1 (forward-sexp 1))
	  (end1 (point)))
      (read-from-string (buffer-substring beg1 end1)))))  

(defun obtain-expansion-term (term)
  (progn 
    (my-clean-acl2)
    (comint-send-string *acl2-buffer-name* (format ":trans! %s\n" term))
    (sit-for 0.1)
    (car (read-expansion))))



;;; Obtain guards from an expresion

(defun init-guards ()
  (comint-send-string *acl2-buffer-name* "(include-book \"misc/bash\" :dir :system)\n"))


(defun bash-guards (l)
  (my-clean-acl2)
  (comint-send-string *acl2-buffer-name* (format "(easy-simplify-term %s)\n" (nested-lists-to-string l)))
  (sit-for 0.1)
  (with-current-buffer *acl2-buffer-name*
    (end-of-buffer)
    (let* ((end1 (point))
	  (s1 (backward-sexp 1))
	  (beg1 (point))
	  (sol (read-from-string (buffer-substring beg1 end1))))
      (while (not (listp (car sol)))
	(progn (setf end1 beg1)
	       (setf s1 (backward-sexp 1))
	       (setf beg1 (point))
	       (setf sol (read-from-string (buffer-substring beg1 end1)))))
      (car sol))))


(defun remove-implies (l)
  (do ((temp l (cdr temp))
       (res nil))
      ((endp temp) (remove-duplicates res :test #'equal))
      (if (equal (caar temp) 'IMPLIES)
	  nil
	(setf res (append res (list (car temp)))))))
      
      



(defun obtain-guards-term (term)
  (bash-guards (obtain-guards (obtain-expansion-term term))))
;  (let ((res  (remove-implies (list ))))
 ;   (if (listp res)
;	(if (< 1 (length res))
;	    (cons 'and res)
;	  res)
 ;     res)))


; (obtain-guards-term '(equal (helper-fact n a) (* (1+ n) a)))



;;;; Add guards of a definition 


(defun add-guards-defun (name args body decl)
  (if decl
      (if (member ':guard (cadr decl))
	  (setf acl2guards (append acl2guards (list  (cons (car (read-from-string (upcase (format "%s" name)))) 
							   (cons (mapcar (lambda (x) (car (read-from-string (upcase (format "%s" x)))))  args)
							   (list (nth 1 (member ':guard (cadr decl)))))))))
	(setf acl2guards (append acl2guards (list  (cons (car (read-from-string (upcase (format "%s" name))))
						     (cons (mapcar (lambda (x) (car (read-from-string (upcase (format "%s" x)))))  args)
							       (list (obtain-guards-term body))))))))
    (setf acl2guards (append acl2guards (list  (cons (car (read-from-string (upcase (format "%s" name))))
						     (cons (mapcar (lambda (x) (car (read-from-string (upcase (format "%s" x)))))  args)
							       (list (obtain-guards-term body)))))))))



(defun obtain-guards-theorem ()
  
  (forward-sexp 1)
  (setf end (point))
  (backward-sexp 1)
  (setf beg (point))
  (setf a (read-from-string (buffer-substring beg end)))
  (cond ((equal (car (read-from-string (upcase (format "%s" (caar a))))) 'DEFTHM)
	 (if (equal (car (read-from-string (upcase (format "%s" (car (nth 2 (car a))))))) 
		    'IMPLIES) 
	     (obtain-guards-term (nth 2 (nth 2 (car a))))
	   (obtain-guards-term (nth 2 (car a)))
	 ))
 
	  (t (format "This only works for theorems"))
	  )

  )





(defvar acl2guards 
  '((BINARY-+ . ((X Y) (AND (ACL2-NUMBERP X) (ACL2-NUMBERP Y))))
   (BINARY-* . ((X Y) (AND (ACL2-NUMBERP X) (ACL2-NUMBERP Y))))
   (ZP . ((X) (NATP X)))
   (APPEND . ((X Y) (TRUE-LISTP X)))
   (IF . ((X Y Z) T))
   (EQUAL . ((X Y) T))
   (quote . (nil T))
   (/= (X Y) (AND (ACL2-NUMBERP X) (ACL2-NUMBERP Y)))
   (< (X Y) (AND (RATIONALP X) (RATIONALP Y)))
   (= (X Y) (AND (ACL2-NUMBERP X) (ACL2-NUMBERP Y)))
   (ABS (X) (RATIONALP X)) (ACONS (KEY DATUM ALIST) (ALISTP ALIST)) (ALPHA-CHAR-P (X) (CHARACTERP X))
   (ASH (I C) (AND (INTEGERP I) (INTEGERP C)))
   (ATOM (X) T)
   (BUTLAST (LST N) (AND (TRUE-LISTP LST) (INTEGERP N) (<= 0 N)))
   (CAR (X) (OR (CONSP X) (EQUAL X NIL)))
   (CDR (X) (OR (CONSP X) (EQUAL X NIL)))
   (CEILING (I J) (AND (RATIONALP I) (RATIONALP J) (NOT (EQL J 0)))) 
   (CHAR (S N) (AND (STRINGP S) (INTEGERP N) (<= 0 N) (< N (LENGTH S)))) 
   (CHAR-CODE (X) (CHARACTERP X))
   (CHAR-DOWNCASE (X) (AND (CHARACTERP X) (STANDARD-CHAR-P X)))
   (CHAR-EQUAL (X Y) (AND (CHARACTERP X) (STANDARD-CHAR-P X) (CHARACTERP Y) (STANDARD-CHAR-P Y)))
   (CHAR-UPCASE (X) (AND (CHARACTERP X) (STANDARD-CHAR-P X))) 
   (CHAR< (X Y) (AND (CHARACTERP X) (CHARACTERP Y))) 
   (CHAR<= (X Y) (AND (CHARACTERP X) (CHARACTERP Y))) 
   (CHAR> (X Y) (AND (CHARACTERP X) (CHARACTERP Y))) 
   (CHAR>= (X Y) (AND (CHARACTERP X) (CHARACTERP Y)))
   (CHARACTERP (X) T)
   (CODE-CHAR (X) (AND (INTEGERP X) (NOT (< X 0)) (< X 256)))
   (COERCE (X Y) (CASE Y (LIST (STRINGP X)) (STRING (CHARACTER-LISTP X)) (OTHERWISE NIL)))
   (COMPLEX (X Y) (AND (RATIONALP X) (RATIONALP Y)))
   (CONJUGATE (X) (ACL2-NUMBERP X))
   (CONS (X Y) T)
   (CONSP (X) T)
   (DENOMINATOR (X) (RATIONALP X))
   (ENDP (X) (OR (CONSP X) (EQUAL X NIL)))
   (EQ (X Y) (OR (SYMBOLP X) (SYMBOLP Y)))
   (EQL (X Y) (OR (EQLABLEP X) (EQLABLEP Y))) 
   (EQUAL (X Y) T) (EVENP (X) (INTEGERP X)) 
   (EXPT (R I) (AND (ACL2-NUMBERP R) (INTEGERP I) (NOT (AND (EQL R 0) (< I 0))))) 
   (FLOOR (I J) (AND (RATIONALP I) (RATIONALP J) (NOT (EQL J 0))))
   (IDENTITY (X) T)
   (IMAGPART (X) (ACL2-NUMBERP X)) 
   (INTEGER-LENGTH (I) (INTEGERP I)) 
   (INTEGERP (X) T)
   (KEYWORDP (X) T)
   (LAST (L) (LISTP L)) 
   (LENGTH (X) (OR (TRUE-LISTP X) (STRINGP X)))
   (LISTP (X) T)
   (LOGANDC1 (I J) (AND (INTEGERP I) (INTEGERP J))) (LOGANDC2 (I J) (AND (INTEGERP I) (INTEGERP J))) (LOGBITP (I J) (AND (INTEGERP J) (INTEGERP I) (<= 0 I))) (LOGCOUNT (X) (INTEGERP X)) (LOGNAND (I J) (AND (INTEGERP I) (INTEGERP J))) (LOGNOR (I J) (AND (INTEGERP I) (INTEGERP J))) (LOGNOT (I) (INTEGERP I)) (LOGORC1 (I J) (AND (INTEGERP I) (INTEGERP J))) (LOGORC2 (I J) (AND (INTEGERP I) (INTEGERP J))) (LOGTEST (X Y) (AND (INTEGERP X) (INTEGERP Y))) (LOWER-CASE-P (X) (AND (CHARACTERP X) (STANDARD-CHAR-P X))) (MAX (X Y) (AND (RATIONALP X) (RATIONALP Y))) (MIN (X Y) (AND (RATIONALP X) (RATIONALP Y))) (MINUSP (X) (RATIONALP X)) (MOD (X Y) (AND (RATIONALP X) (RATIONALP Y) (NOT (EQL Y 0)))) (NOT (P) T) (NTH (N L) (AND (INTEGERP N) (<= 0 N) (TRUE-LISTP L))) (NTHCDR (N L) (AND (INTEGERP N) (<= 0 N) (TRUE-LISTP L))) (NULL (X) T) (NUMERATOR (X) (RATIONALP X)) (ODDP (X) (INTEGERP X)) (PLUSP (X) (RATIONALP X)) (RATIONALP (X) T) (REALPART (X) (ACL2-NUMBERP X)) (REM (X Y) (AND (RATIONALP X) (RATIONALP Y) (NOT (EQL Y 0)))) (REVAPPEND (X Y) (TRUE-LISTP X)) (REVERSE (X) (OR (TRUE-LISTP X) (STRINGP X))) (ROUND (I J) (AND (RATIONALP I) (RATIONALP J) (NOT (EQL J 0)))) (SIGNUM (X) (RATIONALP X)) (STANDARD-CHAR-P (X) (CHARACTERP X)) (STRING (X) (OR (STRINGP X) (SYMBOLP X) (CHARACTERP X))) (STRING-DOWNCASE (X) (AND (STRINGP X) (STANDARD-CHAR-LISTP (COERCE X (quote LIST))))) (STRING-EQUAL (STR1 STR2) (AND (STRINGP STR1) (STANDARD-CHAR-LISTP (COERCE STR1 (quote LIST))) (STRINGP STR2) (STANDARD-CHAR-LISTP (COERCE STR2 (quote LIST))))) (STRING-UPCASE (X) (AND (STRINGP X) (STANDARD-CHAR-LISTP (COERCE X (quote LIST))))) (STRING< (STR1 STR2) (AND (STRINGP STR1) (STRINGP STR2))) (STRING<= (STR1 STR2) (AND (STRINGP STR1) (STRINGP STR2))) (STRING> (STR1 STR2) (AND (STRINGP STR1) (STRINGP STR2))) (STRING>= (STR1 STR2) (AND (STRINGP STR1) (STRINGP STR2))) (STRINGP (X) T) (SUBLIS (ALIST TREE) (EQLABLE-ALISTP ALIST)) (SUBSEQ (SEQ START END) (AND (OR (TRUE-LISTP SEQ) (STRINGP SEQ)) (INTEGERP START) (<= 0 START) (OR (NULL END) (AND (INTEGERP END) (<= END (LENGTH SEQ)))) (<= START (OR END (LENGTH SEQ))))) (SUBST (NEW OLD TREE) (EQLABLEP OLD)) (SUBSTITUTE (NEW OLD SEQ) (OR (AND (STRINGP SEQ) (CHARACTERP NEW)) (AND (TRUE-LISTP SEQ) (OR (EQLABLEP OLD) (EQLABLE-LISTP SEQ))))) (SYMBOL-NAME (X) (SYMBOLP X)) (SYMBOLP (X) T) (TRUNCATE (I J) (AND (RATIONALP I) (RATIONALP J) (NOT (EQL J 0)))) (UPPER-CASE-P (X) (AND (CHARACTERP X) (STANDARD-CHAR-P X))) (ZEROP (X) (ACL2-NUMBERP X))))
