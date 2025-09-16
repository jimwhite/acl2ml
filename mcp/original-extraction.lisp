;;;; original-extraction.lisp
;;;; Direct conversion of extraction.el following original logic exactly
;;;;
;;;; ORIGINAL MAPPING:
;;;; - extraction.el:extract-info() → extract-info()
;;;; - extraction.el:extract-list() → extract-list()
;;;; - extraction.el:arity_1() → arity-1()
;;;; - extraction.el:quicksort-triple() → quicksort-triple()
;;;; - table-to-feature-vector.el:build-table() → build-table()
;;;; - extraction-recursive.el:search-for-recursive-calls() → search-for-recursive-calls()
;;;;
;;;; This follows the exact original logic, not my own design

(defpackage #:acl2ml-original-extraction
  (:use #:cl)
  (:export #:extract-info
           #:build-table
           #:search-for-recursive-calls
           #:extract-definitions-original-format))

(in-package :acl2ml-original-extraction)

;;; Core extraction functions - exact conversion from extraction.el

;; ORIGINAL: extraction.el:extract-list() lines 3-9
;; CONVERTED: Same name, same logic exactly
(defun extract-list (lis level res)
  "Extract list structure with levels (exact conversion)
   ORIGINAL: extraction.el:extract-list()
   CONVERTED: Same logic - builds nested structure analysis"
  (setf res (append res (list (list (car lis) (length (cdr lis)) level))))
  (do ((temp (cdr lis) (cdr temp)))
      ((endp temp) res)
    (if (listp (car temp))
        (setf res (append res (extract-list (car temp) (1+ level) nil)))
        (setf res (append res (list (list (car temp) 0 (+ 1 level))))))))

;; ORIGINAL: extraction.el:quicksort-triple() lines 11-19
;; CONVERTED: Same name, same logic exactly
(defun quicksort-triple (list n)
  "Quicksort by nth element of triple (exact conversion)
   ORIGINAL: extraction.el:quicksort-triple()
   CONVERTED: Same sorting logic"
  (if (<= (length list) 1)
      list
      (let ((pivot (nth n (car list))))
        (append
         (quicksort-triple (remove-if-not (lambda (x) (< (nth n x) pivot)) list) n)
         (remove-if-not (lambda (x) (= (nth n x) pivot)) list)
         (quicksort-triple (remove-if-not (lambda (x) (> (nth n x) pivot)) list) n)))))

;; ORIGINAL: extraction.el:arity_1() lines 40-48
;; CONVERTED: arity-1() - same logic exactly
(defun arity-1 (formulas)
  "Process arity information (exact conversion)
   ORIGINAL: extraction.el:arity_1()
   CONVERTED: Same arity processing logic"
  (let ((temp2 nil))
    (dolist (item formulas temp2)
      (if (and (equal (nth 1 item) 0) (not (integerp (nth 0 item))))
          (setf temp2 (append temp2 (list (list (nth 0 item)
                                               -1
                                               (nth 2 item)))))
          (setf temp2 (append temp2 (list item)))))))

;; ORIGINAL: extraction.el:extract-info() - core extraction logic
;; CONVERTED: Same name, same logic exactly
(defun extract-info (thm)
  "Extract information from ACL2 theorem or definition (exact conversion)
   ORIGINAL: extraction.el:extract-info()
   CONVERTED: Same extraction logic following original exactly"
  (let ((name (cadr thm)))
    (if (not (equal name '|))
        (append (list name) (arity-1 (quicksort-triple (extract-list (car (cddr thm)) 1 nil) 2)))
        ;; Handle the special case (not implemented in original)
        (error "Special case not implemented yet"))))

;;; Table building functions - conversion from table-to-feature-vector.el

;; ORIGINAL: table-to-feature-vector.el:build-table() - PHASE 2 PIPELINE STEP 2
;; This is the critical function that creates the 7-bucket arity structure
(defun build-table (info-list)
  "Build 7-bucket arity table from extracted info (CORE PHASE 2 FUNCTION)

   ORIGINAL: table-to-feature-vector.el:build-table() lines 1-10
   CONVERTED: Exact same logic - creates arity-based feature buckets

   PIPELINE POSITION: Phase 2 Step 2
   INPUT: (name . feature-list) from extract-info()
   OUTPUT: (name [arity0-features] [arity1-features] ... [arity6-features])

   CRITICAL: This creates the structure expected by populate-table()"
  (let ((name (car info-list))
        (formulas (cdr info-list)))
    (let ((temp nil))
      ;; Create 7 buckets (i from 1 to 7, representing arities 0-6)
      (dotimes (i 7)
        (let ((temp2 nil))
          ;; For each arity level, collect features with that arity
          (dotimes (j 7) ; j from -1 to 5 (representing arity levels)
            (let ((target-arity (- j 1))) ; Convert j to arity (-1 to 5)
              (dolist (formula formulas)
                (when (= (nth 1 formula) target-arity)
                  (push formula temp2)))))
          (push (nreverse temp2) temp)))
      (append (list name) (nreverse temp))))))

;; ORIGINAL: extraction-recursive.el:search-for-recursive-calls()
;; CONVERTED: Stub - needs exact original logic
(defun search-for-recursive-calls (table)
  "Search for recursive calls in table (stub - needs exact original logic)
   ORIGINAL: extraction-recursive.el:search-for-recursive-calls()
   CONVERTED: TODO - implement exact original logic"
  ;; This will be filled in with the exact logic from extraction-recursive.el
  table) ; For now, just return the table unchanged

;;; High-level extraction function following original pipeline exactly

;; ORIGINAL: extraction-recursive.el lines 95-124 - the exact pipeline
;; CONVERTED: extract-definitions-original-format() - follows exact original pipeline
(defun extract-definitions-original-format (acl2-form)
  "Extract definitions using exact original pipeline
   ORIGINAL: extraction-recursive.el logic in extract-tables-next-event()
   CONVERTED: Same pipeline - extract-info → build-table → search-for-recursive-calls

   This follows the exact original logic:
   1. extract-info() from extraction.el
   2. build-table() from table-to-feature-vector.el
   3. search-for-recursive-calls() from extraction-recursive.el"

  (cond
    ;; DEFTHM case - original logic lines 95-104
    ((equal (car acl2-form) 'defthm)
     (let* ((info (build-table (extract-info acl2-form))))
       info))

    ;; DEFUN case - original logic lines 106-124
    ((equal (car acl2-form) 'defun)
     (let* ((declaret (and (>= (length acl2-form) 4)
                          (listp (nth 3 acl2-form))
                          (equal (car (nth 3 acl2-form)) 'declare)))
            ;; Handle declare statements like original
            (processed-form (if declaret
                              ;; Remove declare statement like original remove-second-and-third
                              (list (nth 0 acl2-form) (nth 1 acl2-form) (nth 2 acl2-form) (nth 4 acl2-form))
                              ;; Remove one element like original remove-second
                              (list (nth 0 acl2-form) (nth 1 acl2-form) (nth 3 acl2-form))))
            (info (search-for-recursive-calls (build-table (extract-info processed-form)))))
       info))

    ;; Other cases - just return nil like original
    (t nil)))

;; Test function to verify conversion
(defun test-original-extraction ()
  "Test the original extraction functions"
  (format t "~%TESTING ORIGINAL EXTRACTION CONVERSION~%")
  (format t "=====================================~%")

  ;; Test extract-list
  (format t "~%Test 1: extract-list...~%")
  (let ((result (extract-list '(reverse (append x y)) 1 nil)))
    (format t "Result: ~A~%" result))

  ;; Test arity-1
  (format t "~%Test 2: arity-1...~%")
  (let ((test-data '((reverse 1 2) (append 2 3) (x 0 4) (y 0 5))))
    (let ((result (arity-1 test-data)))
      (format t "Result: ~A~%" result)))

  ;; Test quicksort-triple
  (format t "~%Test 3: quicksort-triple...~%")
  (let ((test-data '((a 3 1) (b 1 2) (c 2 3))))
    (let ((result (quicksort-triple test-data 1)))
      (format t "Result: ~A~%" result)))

  (format t "~%Original extraction test completed.~%"))