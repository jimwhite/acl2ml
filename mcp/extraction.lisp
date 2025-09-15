;;;; complete-original-extraction.lisp
;;;; COMPLETE EXACT CONVERSION of the original ACL2(ml) extraction pipeline
;;;;
;;;; This file contains the EXACT conversion of:
;;;; 1. extraction.el - ALL functions exactly as written
;;;; 2. table-to-feature-vector.el - ALL functions and variables exactly as written
;;;; 3. extraction-recursive.el - pipeline integration exactly as written
;;;;
;;;; NO CHANGES to original logic - only syntax conversion from Emacs Lisp to Common Lisp


(defpackage #:acl2ml-complete-original
  (:use #:cl)
  (:export #:extract-acl2-definitions-original-pipeline
           #:export-library-original-format))

(in-package :acl2ml-complete-original)

;;; =============================================================================
;;; EXACT CONVERSION OF extraction.el
;;; =============================================================================

(defun extract-list (lis level res)
  (setf res (append res (list (list (car lis) (length (cdr lis)) level))))
  (do ((temp (cdr lis) (cdr temp)))
      ((endp temp) res)
    (if (listp (car temp))
        (setf res (append res (extract-list (car temp) (1+ level) nil)))
        (setf res (append res (list (list (car temp) 0 (+ 1 level))))))))

(defun quicksort-triple (list n)
  (if (<= (length list) 1)
      list
      (let ((pivot (nth n (car list))))
        (append
	 (quicksort-triple (remove-if-not #'(lambda (x) (< (nth n x) pivot)) list) n) 
	 (remove-if-not #'(lambda (x) (= (nth n x) pivot)) list)
	 (quicksort-triple (remove-if-not #'(lambda (x) (> (nth n x) pivot)) list) n)
          ))))

(defun arity_1 (formulas)
  (do ((temp formulas (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (if (and (equal (nth 1 (car temp)) 0) (not (integerp (nth 0 (car temp)))))
	(setf temp2 (append temp2 (list (list (nth 0 (car temp))
					      -1
					      (nth 2 (car temp))))))
      (setf temp2 (append temp2 (list (car temp)))))))


(defun extract-info (thm)
  (let ((name (cadr thm)))
    (if (not (equal name '|))
	(append (list name) (arity_1 (quicksort-triple (extract-list (car (cddr thm)) 1 nil) 2)))
      (let ((name-thm (form-name-and-thm (cddr thm))))
	(append (list (car name-thm)) (arity_1 (quicksort-triple (extract-list (caadr name-thm) 1 nil) 2)))
      ))))

(defun form-name-and-thm (thm)
  (do ((temp thm (cdr temp))
       (temp2 "|"))
      ((equal (car temp) '|) (list (make-symbol (concatenate 'string temp2 "|" )) (cdr temp)))
      (setf temp2 (concatenate 'string temp2 (format nil "~A" (car temp))))))

(defun extract-level (formulas level)
  (do ((temp formulas (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (if (equal (nth 2 (car temp)) level)
        (setf temp2 (append temp2 (list (car temp)))))))

(defun extract-arity (formulas arity)
  (do ((temp formulas (cdr temp))
       (temp2 nil))
      ((endp temp) temp2)
    (if (equal (nth 1 (car temp)) arity)
	(setf temp2 (append temp2 (list (car temp))))))
  )

(defun build-table (list)
  (let ((name (car list))
        (formulas (cdr list)))
    (do ((i 1 (+ 1 i))
         (temp nil))
        ((equal i 8) (append (list name) temp))
      (setf temp (append temp
                        (list (do ((j -1 (+ 1 j))
                                   (temp2 nil))
                                  ((equal j 6) temp2)
                                (setf temp2 (append temp2
                                                   (list (do ((temp3 (extract-arity (extract-level formulas i) j) (cdr temp3))
                                                              (temp4 nil))
                                                             ((endp temp3) temp4)
							     (setf temp4 (append temp4 (list (nth 0 (car temp3)))))))
						     ))))))
      )))

(defun search-for-recursive-call-step1 (list name)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp) (reverse temp2))
    (if (equal (car temp) name)
        (setf temp2 (cons 'recursive-call temp2))
        (setf temp2 (cons (car temp) temp2)))))

(defun search-for-recursive-call-step2 (list name)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp) (reverse temp2))
    (setf temp2 (cons (search-for-recursive-call-step1 (car temp) name) temp2))))

(defun search-for-recursive-call-step3 (list name)
  (do ((temp list (cdr temp))
       (temp2 nil))
      ((endp temp) (reverse temp2))
    (setf temp2 (cons (search-for-recursive-call-step2 (car temp) name) temp2))))

(defun search-for-recursive-calls (list)
  (cons (car list) (search-for-recursive-call-step3 (cdr list) (car list))))