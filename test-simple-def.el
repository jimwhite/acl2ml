;; Simple test to manually start ACL2 and extract one definition

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

(defun test-single-definition ()
  "Test extracting a single definition with ACL2 running"
  (interactive)
  
  ;; Start ACL2
  (start-acl2-for-testing)
  
  ;; Create a simple test file with one definition
  (let ((test-content "(in-package \"ACL2\")

(defun test-function (x)
  (if (zerop x)
      0
    (+ 1 (test-function (- x 1)))))

(defthm test-theorem
  (implies (natp x)
           (natp (test-function x))))
"))
    
    ;; Write test content to a temporary file
    (with-temp-file "/tmp/test-acl2ml.lisp"
      (insert test-content))
    
    ;; Open the test file
    (find-file "/tmp/test-acl2ml.lisp")
    
    ;; Make sure ACL2ml extraction mode is loaded
    (acl2-extraction-mode)
    
    ;; Clear variables
    (setf definitions nil)
    (setf lemmas nil)
    
    ;; Go to the defun
    (goto-char (point-min))
    (search-forward "(defun test-function")
    (beginning-of-line)
    
    (message "About to extract definition at point...")
    (message "Current line: %s" (thing-at-point 'line))
    
    ;; Try to extract this single definition
    (condition-case err
        (progn
          (extract-tables-next-event)
          (message "SUCCESS!")
          (message "Definitions found: %d" (length definitions))
          (message "Lemmas found: %d" (length lemmas))
          (when definitions
            (message "First definition: %s" (car definitions))))
      (error (message "ERROR: %s" err)))
    
    ;; Clean up
    (kill-buffer)
    (delete-file "/tmp/test-acl2ml.lisp")))

(message "Simple test loaded. Run: M-x test-single-definition")
