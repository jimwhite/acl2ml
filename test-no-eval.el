;; Non-evaluating extraction test - extract definition structure without ACL2

(defun extract-definition-info-only (sexp)
  "Extract information from a definition without evaluating it"
  (let ((form-type (car sexp)))
    (cond 
     ((eq form-type 'defun)
      (let ((name (nth 1 sexp))
            (args (nth 2 sexp))
            (body (nthcdr 3 sexp)))
        (list 'defun name args (length args) body)))
     
     ((eq form-type 'defthm)
      (let ((name (nth 1 sexp))
            (formula (nth 2 sexp)))
        (list 'defthm name formula)))
     
     (t 
      (list 'other form-type)))))

(defun test-non-eval-extraction ()
  "Test extraction without ACL2 evaluation"
  (interactive)
  
  ;; Test with simple expressions
  (let ((test-defun '(defun simple-add (x y) (+ x y)))
        (test-defthm '(defthm add-commutative (equal (+ x y) (+ y x)))))
    
    (message "Testing defun extraction...")
    (let ((defun-info (extract-definition-info-only test-defun)))
      (message "Defun info: %s" defun-info))
    
    (message "Testing defthm extraction...")
    (let ((defthm-info (extract-definition-info-only test-defthm)))
      (message "Defthm info: %s" defthm-info)))
  
  ;; Test with file content
  (message "Testing with temp file...")
  (with-temp-buffer
    (insert "(defun factorial (n)\n")
    (insert "  (if (zp n) 1 (* n (factorial (- n 1)))))\n\n")
    (insert "(defthm factorial-positive\n")
    (insert "  (> (factorial n) 0))\n")
    
    (goto-char (point-min))
    (let ((extracted '()))
      (while (not (eobp))
        (when (looking-at "(")
          (let ((sexp (read (current-buffer))))
            (when (member (car sexp) '(defun defthm))
              (setq extracted (cons (extract-definition-info-only sexp) extracted)))))
        (forward-char 1))
      
      (message "Extracted from buffer: %s" (reverse extracted)))))

(defun test-file-parsing ()
  "Test parsing a real ACL2 file without evaluation"
  (interactive)
  
  (let* ((acl2-books-dir (or (getenv "ACL2_SYSTEM_BOOKS") "/home/acl2/books"))
         (test-files (list
                     (concat acl2-books-dir "/arithmetic-5/lib/basic-ops/basic.lisp")
                     (concat acl2-books-dir "/arithmetic-3/top.lisp")
                     (concat acl2-books-dir "/std/lists/top.lisp")))
         (test-file nil))
    
    ;; Find first existing file
    (dolist (file test-files)
      (when (and (not test-file) (file-exists-p file))
        (setq test-file file)))
    
    (if (not test-file)
        (message "No test files found in %s" acl2-books-dir)
      
      (message "Testing file parsing with: %s" test-file)
      
      (with-temp-buffer
        (insert-file-contents test-file)
        (goto-char (point-min))
        
        (let ((definitions '())
              (theorems '())
              (count 0))
          
          (while (and (not (eobp)) (< count 20)) ; Limit to first 20 forms
            (condition-case err
                (when (looking-at "(")
                  (let ((sexp (read (current-buffer))))
                    (when (listp sexp)
                      (cond 
                       ((eq (car sexp) 'defun)
                        (setq definitions (cons (nth 1 sexp) definitions))
                        (setq count (1+ count)))
                       ((eq (car sexp) 'defthm)
                        (setq theorems (cons (nth 1 sexp) theorems))
                        (setq count (1+ count)))))))
              (error 
               (message "Parse error: %s" err)
               (forward-char 1))))
          
          (message "Found %d definitions: %s" (length definitions) (reverse definitions))
          (message "Found %d theorems: %s" (length theorems) (reverse theorems)))))))
