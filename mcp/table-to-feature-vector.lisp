;;; =============================================================================
;;; Source table-to-feature-vector.el
;;; =============================================================================

(in-package :acl2ml-complete-original)

(defparameter *arity0* nil)
(defparameter *arity1*
  '(("recursive-call" -1)
    ("consp" 1)
    ("integerp" 2)
    ("natp" 3)
    ("endp" 4)))
(defparameter *arity2* '(("append" 1) ("recursive-call" -2)))
(defparameter *arity3* '(("recursive-call" -3)))
(defparameter *arity4* '(("recursive-call" -4)))
(defparameter *arity5* '(("recursive-call" -5)))
(defparameter *n-arity0* 1)
(defparameter *n-arity1* 5)
(defparameter *n-arity2* 2)
(defparameter *n-arity3* 1)
(defparameter *n-arity4* 1)
(defparameter *n-arity5* 1)

(defun get-arity-list (i)
  (cond ((equal i 0) *arity0*)
        ((equal i 1) *arity1*)
        ((equal i 2) *arity2*)
        ((equal i 3) *arity3*)
        ((equal i 4) *arity4*)
        ((equal i 5) *arity5*)))

(defun increase-narity (i)
  (cond ((equal i 0) (setf *n-arity0* (+ 1 *n-arity0*)))
        ((equal i 1) (setf *n-arity1* (+ 1 *n-arity1*)))
        ((equal i 2) (setf *n-arity2* (+ 1 *n-arity2*)))
        ((equal i 3) (setf *n-arity3* (+ 1 *n-arity3*)))
        ((equal i 4) (setf *n-arity4* (+ 1 *n-arity4*)))
        ((equal i 5) (setf *n-arity5* (+ 1 *n-arity5*)))))

(defun get-narity (i)
  (cond ((equal i 0) *n-arity0*)
        ((equal i 1) *n-arity1*)
        ((equal i 2) *n-arity2*)
        ((equal i 3) *n-arity3*)
        ((equal i 4) *n-arity4*)
        ((equal i 5) *n-arity5*)))

(defun convert_arity-1 (list)
  (do ((temp list (cdr temp))
       (temp2 ""))
      ((endp temp) (concatenate 'string "-" temp2))
    (setf temp2 (concatenate 'string temp2 "1"))))

(defun remove-minus (string)
  (let ((minus (search "-" string)))
    (if minus
	(remove-minus (concatenate 'string (subseq string 0 minus) (subseq string (1+ minus))))
        string)))

(defun remove-minus-add-minus (string)
  (if (search "-" string)
      (concatenate 'string "-" (remove-minus string))
      string))

;; Helper function for string-to-number conversion (Emacs Lisp equivalent)
(defun string-to-number (str)
  (parse-integer str :junk-allowed t))

(defun convert (list i)
  (if (equal i -1)
      (string-to-number (convert_arity-1 list))
      (do ((temp list (cdr temp))
           (temp2 ""))
          ((endp temp) (string-to-number (remove-minus-add-minus temp2)))
        (if (assoc (format nil "~s" (car temp)) (get-arity-list i) :test #'string=)
            (setf temp2 (concatenate 'string temp2
                                    (format nil "~s" (cadr (assoc (format nil "~s" (car temp))
                                                                  (get-arity-list i) :test #'string=)))))
            (progn
              (cond
                ((equal i 0) (setf *arity0*
                                  (append *arity0*
                                         (list (list (format nil "~s" (car temp)) *n-arity0*)))))
                ((equal i 1) (setf *arity1*
                                  (append *arity1*
                                         (list (list (format nil "~s" (car temp)) *n-arity1*)))))
                ((equal i 2) (setf *arity2*
                                  (append *arity2*
                                         (list (list (format nil "~s" (car temp)) *n-arity2*)))))
                ((equal i 3) (setf *arity3*
                                  (append *arity3*
                                         (list (list (format nil "~s" (car temp)) *n-arity3*)))))
                ((equal i 4) (setf *arity4*
                                  (append *arity4*
                                         (list (list (format nil "~s" (car temp)) *n-arity4*)))))
                ((equal i 5) (setf *arity5*
                                  (append *arity5*
                                         (list (list (format nil "~s" (car temp)) *n-arity5*))))))
              (increase-narity i)
              (setf temp2 (concatenate 'string temp2
                                      (format nil "~s" (cadr (assoc (format nil "~s" (car temp))
                                                                    (get-arity-list i) :test #'string=))))))))))

(defun populate-list (list)
  (do ((temp list (cdr temp))
       (i -1)
       (temp2 nil))
      ((endp temp) temp2)
    (if (endp (car temp))
        (progn (setf temp2 (append temp2 (list 0))) (setf i (+ i 1)))
      (progn (setf temp2 (append temp2 (list (convert (car temp) i))) )
               (setf i (+ i 1))))))

(defun populate-table (list)
  (let ((name (car list))
        (features (cdr list)))
    (do ((temp features (cdr temp))
         (temp2 nil))
        ((endp temp) (append (list name) (list temp2)))
      (setf temp2 (append temp2 (list (populate-list (car temp))))))))

(defun flat (ll)
  (if (endp ll)
      nil
      (append (car ll) (flat (cdr ll)))))

(defun flatten-table (list)
  (let ((name (car list))
        (features (cadr list)))
    (append (list name) (list (flat features)))))

;;; =============================================================================
;;; INTEGRATION PIPELINE - EXACT CONVERSION OF ORIGINAL WORKFLOW
;;; =============================================================================

(defun extract-acl2-definitions-original-pipeline (acl2-forms-list file-path)
  "Process ACL2 forms using EXACT original pipeline"

  (let ((definitions nil))
    (dolist (acl2-form acl2-forms-list)
      (cond
        ;; DEFTHM case - original logic lines 95-104
        ((and (listp acl2-form) (equal (car acl2-form) 'defthm))
         (handler-case
             (let* ((info (build-table (extract-info acl2-form)))
                    (name (first info))
                    ;; Create file::symbol format exactly like original - using string instead of symbol
                    (namespaced-name (concatenate 'string file-path "::"
                                                     (if (symbolp name) (symbol-name name) (format nil "~A" name)))))
               (push (cons namespaced-name (rest info)) definitions))
           (error (e)
             (format t "Warning - Error processing DEFTHM ~A - ~A~%" (second acl2-form) e))))

        ;; DEFUN case - original logic lines 106-124 with declare handling
        ((and (listp acl2-form) (equal (car acl2-form) 'defun))
         (handler-case
             (let* ((declaret (and (>= (length acl2-form) 4)
                                  (listp (fourth acl2-form))
                                  (equal (car (fourth acl2-form)) 'declare)))
                    ;; Handle declare statements exactly like original remove-second-and-third/remove-second
                    (processed-form (if declaret
                                        ;; remove-second-and-third: keep positions 0,1,2,4
                                        (list (nth 0 acl2-form) (nth 1 acl2-form) (nth 2 acl2-form) (nth 4 acl2-form))
                                        ;; remove-second: keep positions 0,1,3
                                        (list (nth 0 acl2-form) (nth 1 acl2-form) (nth 3 acl2-form))))
                    (info (search-for-recursive-calls (build-table (extract-info processed-form))))
                    (name (first info))
                    ;; Create file::symbol format exactly like original - using string instead of symbol
                    (namespaced-name (concatenate 'string file-path "::"
                                                     (if (symbolp name) (symbol-name name) (format nil "~A" name)))))
               (push (cons namespaced-name (rest info)) definitions))
           (error (e)
             (format t "Warning - Error processing DEFUN ~A - ~A~%" (second acl2-form) e))))

        ;; Other cases - ignore like original
        (t nil)))

    (reverse definitions)))

;;; Integration with file processing
(defun extract-acl2-definitions-from-file-original (file-path)
  "Extract definitions from ACL2 file using ORIGINAL pipeline
   ORIGINAL - Complete integration of original extraction system
   CONVERTED - Read file + parse + extract using original pipeline"

  (handler-case
      (with-open-file (stream file-path :direction :input)
        (let ((acl2-forms nil))
          ;; Read all forms from file
          (loop for form = (read stream nil nil)
                while form
                do (push form acl2-forms))

          ;; Process using original pipeline
          (extract-acl2-definitions-original-pipeline (reverse acl2-forms) file-path)))
    (error (e)
      (format t "Error reading ~A - ~A~%" file-path e)
      nil)))

;;; Final export function with original format
(defun export-library-original-format (file-path)
  "Export library using ORIGINAL format - creates definitions files exactly like original
   ORIGINAL - storage.el export-library() with complete original pipeline
   CONVERTED - Same functionality producing exact original format"

  (let ((definitions (extract-acl2-definitions-from-file-original file-path)))
    (when definitions
      (format t "Extracted ~A definitions using ORIGINAL pipeline from ~A~%"
              (length definitions) file-path)
      definitions)))

