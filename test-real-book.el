;; Test with actual ACL2 book content

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

(defun test-real-book-extraction ()
  "Test extraction on a comprehensive ACL2 example"
  (interactive)
  
  ;; Start ACL2
  (start-acl2-for-testing)
  
  ;; Create comprehensive test content
  (let ((test-content "(in-package \"ACL2\")

(defun my-append (x y)
  (if (endp x)
      y
    (cons (car x) (my-append (cdr x) y))))

(defthm my-append-nil
  (equal (my-append x nil) x))

(include-book \"std/lists/top\" :dir :system)

(defun reverse-onto (x y)
  (if (endp x)
      y
    (reverse-onto (cdr x) (cons (car x) y))))
"))
    
    ;; Write test content to a temporary file
    (with-temp-file "/tmp/real-test.lisp"
      (insert test-content))
    
    ;; Open the test file
    (find-file "/tmp/real-test.lisp")
    
    ;; Enable ACL2 extraction mode
    (acl2-extraction-mode)
    
    ;; Clear variables
    (setf definitions nil)
    (setf lemmas nil)
    (setf included-libraries nil)
    (setf included-libraries-system nil)
    
    ;; Test comprehensive extraction
    (goto-char (point-min))
    (message "=== COMPREHENSIVE EXTRACTION TEST ===")
    
    ;; Skip the in-package
    (forward-sexp 1)
    
    ;; Extract each form
    (let ((forms '("defun my-append" "defthm my-append-nil" "include-book" "defun reverse-onto")))
      (dolist (form-name forms)
        (condition-case err
            (progn
              (extract-tables-next-event)
              (message "Successfully extracted: %s" form-name))
          (error (message "ERROR extracting %s: %s" form-name err)))))
    
    (message "Final comprehensive results:")
    (message "Definitions: %d" (length definitions))
    (message "Lemmas: %d" (length lemmas))
    (message "Include books: %d system, %d user" 
             (length included-libraries-system)
             (length included-libraries))
    (when definitions
      (message "Definition names: %s" (mapcar 'car definitions)))
    (when lemmas
      (message "Lemma names: %s" (mapcar 'car lemmas)))
    (when included-libraries-system
      (message "System libraries: %s" included-libraries-system))
    
    ;; Clean up
    (kill-buffer)
    (delete-file "/tmp/real-test.lisp")))

(message "Real book test loaded. Run: M-x test-real-book-extraction")
