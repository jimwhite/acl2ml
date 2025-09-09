;; Simple test script to debug ACL2(ml) definition extraction
;; Tests with a single arithmetic book

(defun test-arithmetic-definitions ()
  "Test definition extraction with a single arithmetic book"
  (interactive)
  (let* ((acl2-books-dir (or (getenv "ACL2_SYSTEM_BOOKS") "/home/acl2/books"))
         (test-book (concat acl2-books-dir "/arithmetic-5/lib/basic-ops/building-blocks-helper.lisp")))
    
    (if (not (file-exists-p test-book))
        (message "Test book not found: %s" test-book)
      
      (message "Testing definition extraction with: %s" test-book)
      
      ;; Clear variables
      (setf definitions nil)
      (setf lemmas nil)
      (setf defs-names nil)
      (setf lemma-names nil)
      
      ;; Open the test book
      (find-file test-book)
      (message "Opened book, buffer size: %d" (buffer-size))
      
      ;; Go to beginning
      (beginning-of-buffer)
      
      ;; Try to extract a single event first
      (message "Trying to extract single event...")
      (condition-case err
          (progn
            (extract-tables-next-event)
            (message "After single event: definitions=%s, lemmas=%s" definitions lemmas))
        (error (message "Error in single event extraction: %s" err)))
      
      ;; Reset and try full extraction
      (beginning-of-buffer)
      (setf definitions nil)
      (setf lemmas nil)
      
      (message "Trying full extraction...")
      (condition-case err
          (progn
            (extract-tables-recursive)
            (message "After full extraction: definitions=%d, lemmas=%d" 
                     (length definitions) (length lemmas))
            
            (when definitions
              (message "First definition: %s" (car definitions)))
            
            (when lemmas
              (message "First lemma: %s" (car lemmas))))
        (error (message "Error in full extraction: %s" err)))
      
      ;; Close buffer
      (kill-buffer))))

(defun test-simple-defun ()
  "Test with a simple defun to see if basic extraction works"
  (interactive)
  
  ;; Create a test buffer with simple ACL2 content
  (with-temp-buffer
    (insert "(in-package \"ACL2\")\n\n")
    (insert "(defun test-add (x y)\n")
    (insert "  (+ x y))\n\n")
    (insert "(defthm test-add-commutative\n")
    (insert "  (equal (test-add x y) (test-add y x)))\n")
    
    (write-file "/tmp/test-acl2.lisp"))
  
  ;; Now test extraction on this simple file
  (message "Testing with simple test file...")
  
  (setf definitions nil)
  (setf lemmas nil)
  
  (find-file "/tmp/test-acl2.lisp")
  (beginning-of-buffer)
  
  (condition-case err
      (progn
        (extract-tables-recursive)
        (message "Simple test results: definitions=%d, lemmas=%d" 
                 (length definitions) (length lemmas))
        
        (when definitions
          (message "Test definitions: %s" definitions))
        
        (when lemmas
          (message "Test lemmas: %s" lemmas)))
    (error (message "Error in simple test: %s" err)))
  
  (kill-buffer))

(defun debug-extraction-step-by-step ()
  "Debug the extraction process step by step"
  (interactive)
  
  (let* ((acl2-books-dir (or (getenv "ACL2_SYSTEM_BOOKS") "/home/acl2/books"))
         (test-book (concat acl2-books-dir "/arithmetic-5/lib/basic-ops/common.lisp")))
    
    (if (not (file-exists-p test-book))
        (message "Test book not found: %s" test-book)
      
      (message "Step-by-step debugging with: %s" test-book)
      
      ;; Initialize variables
      (setf definitions nil)
      (setf lemmas nil)
      (setf rep nil)
      
      (find-file test-book)
      (beginning-of-buffer)
      
      ;; Try to find first s-expression
      (message "Looking for first s-expression...")
      (condition-case err
          (progn
            (forward-sexp 1)
            (setf end (point))
            (backward-sexp 1)
            (setf beg (point))
            (let ((expr (buffer-substring beg end)))
              (message "First expression: %s" (substring expr 0 (min 100 (length expr))))
              
              ;; Try to parse it
              (condition-case parse-err
                  (let ((parsed (read-from-string expr)))
                    (message "Parsed first element: %s" (car parsed))
                    
                    ;; Check if it's a defun or defthm
                    (when (listp (car parsed))
                      (let ((form-type (caar parsed)))
                        (message "Form type: %s" form-type)
                        
                        (cond 
                         ((equal (upcase (format "%s" form-type)) "DEFUN")
                          (message "Found DEFUN!"))
                         ((equal (upcase (format "%s" form-type)) "DEFTHM")
                          (message "Found DEFTHM!"))
                         (t (message "Other form: %s" form-type))))))
                (error (message "Parse error: %s" parse-err)))))
        (error (message "Error finding s-expression: %s" err)))
      
      (kill-buffer))))

;; Quick test to see if key functions are available
(defun test-acl2ml-functions ()
  "Test if ACL2ml functions are available"
  (interactive)
  
  (message "Testing ACL2ml function availability...")
  
  (if (fboundp 'extract-tables-recursive)
      (message "✓ extract-tables-recursive is available")
    (message "✗ extract-tables-recursive is NOT available"))
  
  (if (fboundp 'extract-tables-next-event)
      (message "✓ extract-tables-next-event is available")
    (message "✗ extract-tables-next-event is NOT available"))
  
  (if (fboundp 'extract-info)
      (message "✓ extract-info is available")
    (message "✗ extract-info is NOT available"))
  
  (if (boundp 'definitions)
      (message "✓ definitions variable is bound")
    (message "✗ definitions variable is NOT bound"))
  
  (if (boundp 'lemmas)
      (message "✓ lemmas variable is bound")
    (message "✗ lemmas variable is NOT bound")))
