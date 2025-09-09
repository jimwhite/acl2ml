;; Test script to extract definitions from arithmetic book with proper ACL2 setup
;; This will start ACL2 and then extract definitions

(defun test-with-acl2-running ()
  "Test definition extraction with ACL2 properly running"
  (interactive)
  (message "Starting ACL2ml...")
  
  ;; Start ACL2ml which sets up the ACL2 process
  (start-acl2ml)
  
  ;; Wait for ACL2 to start up
  (message "Waiting for ACL2 to initialize...")
  (sit-for 5)  ; Give ACL2 time to start
  
  ;; Now try to extract from arithmetic book
  (let ((test-file "/home/acl2/books/arithmetic-5/lib/basic-ops/common.lisp"))
    (if (file-exists-p test-file)
        (progn
          (message "Opening file: %s" test-file)
          (find-file test-file)
          
          ;; Clear variables
          (setf definitions nil)
          (setf lemmas nil)
          
          ;; Go to beginning and try to extract first definition
          (goto-char (point-min))
          
          ;; Find first defun
          (if (search-forward "(defun " nil t)
              (progn
                (beginning-of-line)
                (message "Found defun at line %d" (line-number-at-pos))
                (message "Trying to extract this definition...")
                
                ;; Try extract-tables-next-event
                (condition-case err
                    (progn
                      (extract-tables-next-event)
                      (message "SUCCESS: Extracted %d definitions" (length definitions))
                      (when definitions
                        (message "First definition: %s" (car definitions))))
                  (error (message "ERROR in extract-tables-next-event: %s" err))))
            (message "No defun found in file"))
          
          (kill-buffer))
      (message "Test file not found: %s" test-file))))

(defun test-arithmetic-simple ()
  "Test with a simpler arithmetic file"
  (interactive)
  (message "Testing with simpler arithmetic file...")
  
  ;; Start ACL2ml
  (start-acl2ml)
  (sit-for 5)
  
  ;; Try with the top-level arithmetic file
  (let ((test-file "/home/acl2/books/arithmetic-5/top.lisp"))
    (if (file-exists-p test-file)
        (progn
          (message "Opening arithmetic-5 top file...")
          (find-file test-file)
          
          ;; Clear variables
          (setf definitions nil)
          (setf lemmas nil)
          
          ;; Try to extract all from this file
          (goto-char (point-min))
          (message "Trying extract-tables-recursive on entire file...")
          
          (condition-case err
              (progn
                (extract-tables-recursive)
                (message "SUCCESS: Found %d definitions, %d lemmas" 
                        (length definitions) (length lemmas))
                (when definitions
                  (message "Sample definitions: %s" (mapcar 'car (seq-take definitions 3))))
                (when lemmas
                  (message "Sample lemmas: %s" (mapcar 'car (seq-take lemmas 3)))))
            (error (message "ERROR in extract-tables-recursive: %s" err)))
          
          (kill-buffer))
      (message "File not found: %s" test-file))))

;; Interactive test to check ACL2 buffer status
(defun check-acl2-status ()
  "Check if ACL2 is properly running"
  (interactive)
  (message "Checking ACL2 status...")
  
  (if (boundp '*acl2-buffer-name*)
      (message "ACL2 buffer name: %s" *acl2-buffer-name*)
    (message "ACL2 buffer name not defined"))
  
  (if (boundp 'inferior-acl2-buffer)
      (message "Inferior ACL2 buffer: %s" inferior-acl2-buffer)
    (message "Inferior ACL2 buffer not defined"))
  
  ;; Check if buffers exist
  (if (get-buffer "*inferior-acl2*")
      (message "Buffer *inferior-acl2* exists")
    (message "Buffer *inferior-acl2* does NOT exist"))
  
  (if (get-buffer "*acl2*")
      (message "Buffer *acl2* exists")
    (message "Buffer *acl2* does NOT exist"))
  
  ;; List all buffers with 'acl2' in the name
  (let ((acl2-buffers (seq-filter (lambda (buf) 
                                   (string-match-p "acl2" (buffer-name buf)))
                                 (buffer-list))))
    (if acl2-buffers
        (message "ACL2-related buffers: %s" (mapcar 'buffer-name acl2-buffers))
      (message "No ACL2-related buffers found"))))

(message "Test functions loaded. Run:")
(message "  M-x test-with-acl2-running")
(message "  M-x test-arithmetic-simple") 
(message "  M-x check-acl2-status")
