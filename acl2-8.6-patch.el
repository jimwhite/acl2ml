;; Fixed extraction-recursive.el for ACL2 8.6 compatibility
;; This file patches the extract-tables-next-event function to handle defun properly

(defun extract-defun-info-simple (defun-form)
  "Extract basic information from a defun form for ACL2 8.6"
  (let ((name (nth 1 defun-form))
        (params (nth 2 defun-form))
        (body (if (and (listp (nth 3 defun-form)) 
                       (equal (car (nth 3 defun-form)) 'declare))
                  (nth 4 defun-form)  ; Has declare, body is 4th element
                (nth 3 defun-form)))) ; No declare, body is 3rd element
    ;; Return a simplified structure compatible with the existing code
    (list name 'defun (length params) body)))

(defun extract-tables-next-event-fixed ()
  "Fixed version of extract-tables-next-event for ACL2 8.6 compatibility"
  (setq point (point))
  (setf rep nil)
  
  ;; We remove the file tmp.out and the redirect ACL2 output to that file
  (let* ((buf (buffer-name))
         (name (if (cl-search "." buf) (cl-subseq buf 0 (cl-search "." buf)) buf)))
    (forward-sexp 1)
    (setf end (point))
    (backward-sexp 1)
    (setf beg (point))
    (setf a (read-from-string (buffer-substring beg end)))
    (forward-sexp 1)
    
    (let ((form-type (car (read-from-string (upcase (format "%s" (caar a)))))))
      (cond ((equal form-type 'DEFTHM)
             ;; Use original logic for defthm
             (let* ((info (build-table (extract-info (car a))))
                    (lemmaname (car info)))
               (progn 
                 (if (member lemmaname (extract-names))
                     (setq rep t)
                   (setq lemmas (append lemmas (list info))))
                 
                 ;; Skip the problematic ACL2 evaluation for now in batch mode
                 (when (and (boundp '*acl2-buffer-name*) 
                            (get-buffer *acl2-buffer-name*))
                   (save-excursion (eval-this-event)))
                 (setq lemma-names (append lemma-names (list lemmaname)))
                 (setq statem (append statem (list (car a)))))))

            ((equal form-type 'DEFUN)
             ;; Use simplified logic for defun to avoid the ACL2 symbol issue
             (let* ((info (extract-defun-info-simple (car a)))
                    (definitionname (car info)))
               (progn 
                 (if (member definitionname (extract-defs-names))
                     (setq rep t)
                   (setq definitions (append definitions (list info))))
                 
                 ;; Skip the problematic ACL2 evaluation for now in batch mode
                 (when (and (boundp '*acl2-buffer-name*) 
                            (get-buffer *acl2-buffer-name*))
                   (save-excursion (eval-this-event))
                   (sit-for 1)
                   ;; Skip guard-related processing for now
                   )
                 (setq defs-names (append defs-names (list definitionname)))
                 (setq defs-statem (append defs-statem (list (car a)))))))

            ((equal form-type 'INCLUDE-BOOK)
             (progn 
               ;; Skip the problematic ACL2 evaluation for now in batch mode
               (when (and (boundp '*acl2-buffer-name*) 
                          (get-buffer *acl2-buffer-name*))
                 (save-excursion (eval-this-event)))
               (if (equal (nth 3 (car a)) :system)
                   (setf included-libraries-system (append included-libraries-system (list (cadar a))))
                 (setf included-libraries (append included-libraries (list (cadar a)))))
               ;; Skip guard import for now
               ))

            ((equal form-type 'DEFMACRO)
             (progn 
               ;; Skip the problematic ACL2 evaluation for now in batch mode
               (when (and (boundp '*acl2-buffer-name*) 
                          (get-buffer *acl2-buffer-name*))
                 (save-excursion (eval-this-event))
                 (sit-for 1))
               ;; Simplified macro handling
               (let* ((info (list (nth 1 (car a)) 'defmacro 0 (nth 3 (car a))))
                      (definitionname (car info)))
                 (if (member definitionname (extract-defs-names))
                     (setq rep t)
                   (setq definitions (append definitions (list info)))))))

            (t
             ;; Skip unknown forms
             (message "Skipping unknown form type: %s" form-type))))))

;; Now replace the original function
(fset 'extract-tables-next-event 'extract-tables-next-event-fixed)

(message "ACL2 8.6 compatibility patch applied to extract-tables-next-event")
