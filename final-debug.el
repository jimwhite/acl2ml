;; Final debug test - simple defun only

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

(defun final-debug-test ()
  "Final debug test - simple defun only"
  (interactive)
  
  ;; Start ACL2
  (start-acl2-for-testing)
  
  ;; Create a simple test file with ONLY a defun - no in-package
  (let ((test-content "(defun test-function (x)
  (if (zerop x)
      0
    (+ 1 (test-function (- x 1)))))
"))
    
    ;; Write test content to a temporary file
    (with-temp-file "/tmp/final-debug.lisp"
      (insert test-content))
    
    ;; Open the test file
    (find-file "/tmp/final-debug.lisp")
    
    ;; Enable ACL2 extraction mode
    (acl2-extraction-mode)
    
    ;; Clear variables
    (setf definitions nil)
    (setf lemmas nil)
    
    ;; Position at the very beginning
    (goto-char (point-min))
    
    (message "=== FINAL DEBUG TEST ===")
    (message "Point is at: %d" (point))
    (message "Current char: %s" (char-after))
    (message "Current line: %s" (thing-at-point 'line))
    
    ;; Let's see what the read will be
    (let* ((point (point))
           (beg nil)
           (end nil)
           (a nil))
      
      ;; Move forward over one S-expression
      (condition-case err
          (progn
            (forward-sexp 1)
            (setf end (point))
            (backward-sexp 1)
            (setf beg (point))
            (let ((text (buffer-substring beg end)))
              (message "Text to read: %s" text)
              (setf a (read-from-string text))
              (message "Read result: %s" a)
              (message "Car of a: %s" (car a))
              (message "Caar of a: %s" (caar a))
              (message "Type of caar: %s" (type-of (caar a)))
              (message "Upcase format of caar: %s" (upcase (format "%s" (caar a))))
              (let ((upcase-str (upcase (format "%s" (caar a)))))
                (message "Read-from-string of upcase: %s" (read-from-string upcase-str))
                (message "Car of read-from-string: %s" (car (read-from-string upcase-str))))))
        (error (message "ERROR reading S-expression: %s" err)))
      
      ;; Try the extraction
      (condition-case err
          (progn
            (message "Attempting to extract...")
            (extract-tables-next-event)
            (message "SUCCESS! Extracted %d definitions, %d lemmas" 
                     (length definitions) (length lemmas)))
        (error (message "ERROR in extraction: %s" err))))
    
    ;; Clean up
    (kill-buffer)
    (delete-file "/tmp/final-debug.lisp")))

(message "Final debug test loaded. Run: M-x final-debug-test")
