;; Minimal test for definition extraction from arithmetic books

(defun test-arithmetic-simple ()
  "Test extraction from a simple arithmetic book"
  (interactive)
  
  ;; Load ACL2ml functions first
  (message "Loading ACL2ml extraction mode...")
  (acl2-extraction-mode)
  
  ;; Now check if ACL2ml is loaded
  (message "Checking ACL2ml availability...")
  (if (not (fboundp 'extract-tables-next-event))
      (message "ERROR: ACL2ml still not loaded!")
    
    ;; Find a simple arithmetic book
    (let* ((acl2-books-dir (or (getenv "ACL2_SYSTEM_BOOKS") "/home/acl2/books"))
           (possible-books (list
                           (concat acl2-books-dir "/arithmetic/top.lisp")
                           (concat acl2-books-dir "/arithmetic-3/top.lisp")
                           (concat acl2-books-dir "/arithmetic-5/top.lisp")
                           (concat acl2-books-dir "/arithmetic-5/lib/basic-ops/common.lisp")))
           (test-book nil))
      
      ;; Find first existing book
      (dolist (book possible-books)
        (when (and (not test-book) (file-exists-p book))
          (setq test-book book)))
      
      (if (not test-book)
          (message "ERROR: No arithmetic books found in %s" acl2-books-dir)
        
        (message "Testing with: %s" test-book)
        
        ;; Clear state
        (setf definitions nil)
        (setf lemmas nil)
        
        ;; Open the book
        (find-file test-book)
        (message "Opened book: %s (size: %d chars)" test-book (buffer-size))
        
        ;; Look at first few lines
        (beginning-of-buffer)
        (let ((first-lines (buffer-substring (point) (min (+ (point) 500) (point-max)))))
          (message "First 500 chars:\n%s" first-lines))
        
        ;; Try processing just the first event
        (beginning-of-buffer)
        (condition-case err
            (progn
              (message "Attempting to extract first event...")
              (extract-tables-next-event)
              (message "SUCCESS: Extracted first event")
              (message "Definitions after first event: %s" definitions)
              (message "Lemmas after first event: %s" lemmas))
          (error (message "ERROR extracting first event: %s" err)))
        
        (kill-buffer)))))

;; Even simpler test with manual content
(defun test-manual-defun ()
  "Test with manually created simple defun"
  (interactive)
  
  ;; Load ACL2ml functions first
  (message "Loading ACL2ml extraction mode...")
  (acl2-extraction-mode)
  
  (if (not (fboundp 'extract-tables-next-event))
      (message "ERROR: ACL2ml not loaded!")
    
    ;; Create a buffer with just a simple defun
    (with-temp-buffer
      (insert "(defun simple-add (x y)\n  (+ x y))\n")
      
      ;; Save to temp file and open it
      (write-file "/tmp/simple-test.lisp")
      (kill-buffer))
    
    ;; Now test with this simple file
    (message "Testing with simple manual defun...")
    
    (setf definitions nil)
    (setf lemmas nil)
    
    (find-file "/tmp/simple-test.lisp")
    (beginning-of-buffer)
    
    (message "Buffer content: '%s'" (buffer-string))
    
    (condition-case err
        (progn
          (extract-tables-next-event)
          (message "Manual test - Definitions: %s" definitions)
          (message "Manual test - Lemmas: %s" lemmas))
      (error (message "ERROR in manual test: %s" err)))
    
    (kill-buffer)))
