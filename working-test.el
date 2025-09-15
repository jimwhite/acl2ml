;; Working test with extraction fix

(defun start-acl2-for-testing ()
  "Start ACL2 without the full ACL2ml interface for testing"
  (message "Loading ACL2 interface...")
  
  ;; Load the ACL2 interface files
  (condition-case err
      (progn
        (load-file "/home/acl2/books/interface/emacs/acl2-mode.el")
        (load-file "/home/acl2/books/interface/emacs/inf-acl2.el")
        (message "ACL2 interface loaded successfully"))
    (error (message "Error loading ACL2 interface: %s" err)))
  
  ;; Set ACL2 program path
  (setq inferior-acl2-program "/home/acl2/saved_acl2")
  
  ;; Start ACL2 process
  (condition-case err
      (progn
        (inferior-acl2 inferior-acl2-program)
        (message "ACL2 process started")
        ;; Give it time to initialize
        (sit-for 3)
        (message "ACL2 ready for use"))
    (error (message "Error starting ACL2: %s" err))))

(defun extract-defun-info (defun-form)
  "Extract information from a defun form - simplified for ACL2 8.6"
  (let ((name (nth 1 defun-form))
        (params (nth 2 defun-form))
        (body (nth 3 defun-form)))
    ;; Return a simple structure for now
    (list name 'defun (length params) body)))

(defun extract-tables-next-event-fixed ()
  "Fixed version of extract-tables-next-event that handles defun properly"
  (setq point (point))
  (setf rep nil)
  
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
             (message "Processing DEFTHM: %s" (nth 1 (car a)))
             ;; Use original logic for defthm
             (let* ((info (build-table (extract-info (car a))))
                    (lemmaname (car info)))
               (if (member lemmaname (extract-names))
                   (setq rep t)
                 (setq lemmas (append lemmas (list info))))
               (message "Added lemma: %s" lemmaname)))
            
            ((equal form-type 'DEFUN)
             (message "Processing DEFUN: %s" (nth 1 (car a)))
             ;; Use simplified logic for defun
             (let* ((info (extract-defun-info (car a)))
                    (definitionname (car info)))
               (if (member definitionname (extract-defs-names))
                   (setq rep t)
                 (setq definitions (append definitions (list info))))
               (message "Added definition: %s" definitionname)))
            
            ((equal form-type 'INCLUDE-BOOK)
             (message "Processing INCLUDE-BOOK: %s" (nth 1 (car a)))
             ;; Handle include-book
             (if (equal (nth 3 (car a)) :system)
                 (setf included-libraries-system (append included-libraries-system (list (nth 1 (car a)))))
               (setf included-libraries (append included-libraries (list (nth 1 (car a)))))))
            
            (t
             (message "Skipping form type: %s" form-type))))))

(defun working-extraction-test ()
  "Test extraction with the fixed function"
  (interactive)
  
  ;; Start ACL2
  (start-acl2-for-testing)
  
  ;; Create test content with both defun and defthm
  (let ((test-content "(defun test-function (x)
  (if (zerop x)
      0
    (+ 1 (test-function (- x 1)))))

(defun another-function (y z)
  (+ y z))
"))
    
    ;; Write test content to a temporary file
    (with-temp-file "/tmp/working-test.lisp"
      (insert test-content))
    
    ;; Open the test file
    (find-file "/tmp/working-test.lisp")
    
    ;; Enable ACL2 extraction mode
    (acl2-extraction-mode)
    
    ;; Clear variables
    (setf definitions nil)
    (setf lemmas nil)
    (setf included-libraries nil)
    (setf included-libraries-system nil)
    
    ;; Test extraction on first defun
    (goto-char (point-min))
    (message "=== WORKING EXTRACTION TEST ===")
    
    (condition-case err
        (progn
          (extract-tables-next-event-fixed)
          (message "First extraction successful!"))
      (error (message "ERROR in first extraction: %s" err)))
    
    ;; Test extraction on second defun
    (condition-case err
        (progn
          (extract-tables-next-event-fixed)
          (message "Second extraction successful!"))
      (error (message "ERROR in second extraction: %s" err)))
    
    (message "Final results:")
    (message "Definitions: %d" (length definitions))
    (message "Lemmas: %d" (length lemmas))
    (when definitions
      (message "Definition names: %s" (mapcar 'car definitions)))
    
    ;; Clean up
    (kill-buffer)
    (delete-file "/tmp/working-test.lisp")))

(message "Working test loaded. Run: M-x working-extraction-test")
