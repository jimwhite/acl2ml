;; Debug test for extraction process

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
    (error (message "Error starting ACL2: %s" err)))

(defun debug-extraction-step-by-step ()
  "Debug the extraction process step by step"
  (interactive)
  
  ;; Start ACL2
  (start-acl2-for-testing)
  
  ;; Create a simple test file with complete definitions
  (let ((test-content "(in-package \"ACL2\")))))

(defun test-function (x)
  (if (zerop x)
      0
    (+ 1 (test-function (- x 1)))))

(defthm test-theorem
  (implies (natp x)
           (natp (test-function x)))
    
    ;; Write test content to a temporary file
    (with-temp-file "/tmp/debug-acl2ml.lisp"
      (insert test-content))
    
    ;; Open the test file
    (find-file "/tmp/debug-acl2ml.lisp")
    
    ;; Enable ACL2 extraction mode
    (acl2-extraction-mode)
    
    ;; Clear variables
    (setf definitions nil)
    (setf lemmas nil)
    
    ;; Position at the defun and debug what we're reading
    (goto-char (point-min))
    (search-forward "(defun test-function")
    (beginning-of-line)
    
    (message "=== DEBUGGING EXTRACTION ===")
    (message "Point is at: %d" (point))
    (message "Current line: %s" (thing-at-point 'line))
    
    ;; Let's manually do what extract-tables-next-event does
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
            (message "Reading from %d to %d" beg end)
            (let ((text (buffer-substring beg end)))
              (message "Text to read: %s" text)
              (setf a (read-from-string text))
              (message "Read result: %s" a)
              (message "Car of read result: %s" (car a))
              (message "Caar of read result: %s" (caar a))
              (message "Type of caar: %s" (type-of (caar a)))))
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
    (delete-file "/tmp/debug-acl2ml.lisp")))

(message "Debug test loaded. Run: M-x debug-extraction-step-by-step")
