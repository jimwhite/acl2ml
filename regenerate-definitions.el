;; Script to regenerate ACL2(ml) definitions for ACL2 8.6
;; 
;; Usage:
;; 1. Load this script in Emacs with ACL2(ml) loaded
;; 2. Run: M-x regenerate-acl2ml-definitions
;; 3. This will process all certified ACL2 books and create new definition files

(defun regenerate-acl2ml-definitions ()
  "Regenerate definition files for all certified ACL2 books"
  (interactive)
  (let* ((acl2-books-dir (or (getenv "ACL2_SYSTEM_BOOKS") "/home/acl2/books"))
         (certified-books (get-certified-acl2-books acl2-books-dir)))
    
    (if (not (file-directory-p acl2-books-dir))
        (error "ACL2_SYSTEM_BOOKS directory not found: %s" acl2-books-dir)
      
      (message "Starting regeneration of ACL2(ml) definitions...")
      (message "Using ACL2_SYSTEM_BOOKS: %s" acl2-books-dir)
      (message "Found %d certified books to process" (length certified-books))
      
      ;; Create new definitions directory if it doesn't exist
      (unless (file-exists-p (concat *home-dir* "definitions/global-new/"))
        (make-directory (concat *home-dir* "definitions/global-new/") t))
      
      ;; Process each certified book
      (let ((processed 0)
            (errors 0))
        (dolist (book certified-books)
          (message "Processing (%d/%d): %s" (1+ processed) (length certified-books) book)
          (condition-case err
              (progn
                (regenerate-book-definitions book acl2-books-dir)
                (setq processed (1+ processed)))
            (error 
             (message "Error processing %s: %s" book err)
             (setq errors (1+ errors)))))
        
        (message "Regeneration complete!")
        (message "Processed: %d books, Errors: %d" processed errors)
        (message "New definitions in definitions/global-new/")))))

(defun get-certified-acl2-books (books-dir)
  "Get list of all certified ACL2 books (those with .cert files)"
  (let ((cert-files (directory-files-recursively books-dir "\\.cert$"))
        (book-files '()))
    
    ;; Convert .cert files to corresponding .lisp files
    (dolist (cert-file cert-files)
      (let ((lisp-file (concat (file-name-sans-extension cert-file) ".lisp")))
        (when (file-exists-p lisp-file)
          (push lisp-file book-files))))
    
    (message "Found %d .cert files, %d corresponding .lisp files" 
             (length cert-files) (length book-files))
    book-files))

(defun regenerate-book-definitions (book-path books-base-dir)
  "Process a single ACL2 book and generate its definitions"
  (let* ((relative-path (file-relative-name book-path books-base-dir))
         (book-name (file-name-sans-extension (file-name-nondirectory book-path)))
         (output-name (replace-regexp-in-string "/" "___" 
                                              (file-name-sans-extension relative-path))))
    
    ;; Open the book file
    (find-file book-path)
    
    ;; Extract definitions (this uses the existing ACL2(ml) extraction)
    (setf definitions nil)
    (setf lemmas nil)
    
    ;; Set up for extraction
    (beginning-of-buffer)
    (condition-case err
        (progn
          ;; Process the file
          (extract-tables-recursive)
          
          ;; Save definitions to new location if we found any
          (when definitions
            (let ((output-file (concat *home-dir* "definitions/global-new/" output-name)))
              (with-temp-file output-file
                (insert (format "%s" definitions)))
              (message "  -> Saved %d definitions to %s" (length definitions) output-name))))
      
      (error (message "  -> Error extracting from %s: %s" relative-path err)))
    
    ;; Close the book file
    (kill-buffer)))

;; Helper function to update the acl2files list with current book structure
(defun generate-new-acl2files-list ()
  "Generate a new acl2files list based on current certified ACL2 books"
  (interactive)
  (let* ((acl2-books-dir (or (getenv "ACL2_SYSTEM_BOOKS") "/home/acl2/books"))
         (certified-books (get-certified-acl2-books acl2-books-dir))
         (relative-paths '()))
    
    ;; Convert to relative paths
    (dolist (book certified-books)
      (let ((relative-path (file-relative-name book acl2-books-dir)))
        (push (concat "/" relative-path) relative-paths)))
    
    ;; Sort the paths for better organization
    (setq relative-paths (sort relative-paths 'string<))
    
    ;; Generate the new list and display it
    (with-temp-buffer 
      (insert ";; Updated acl2files list for ACL2 8.6\n")
      (insert ";; Generated from certified books in " acl2-books-dir "\n")
      (insert ";; Total books: " (format "%d" (length relative-paths)) "\n\n")
      (insert "(setq new-acl2files\n  '(")
      (dolist (file relative-paths)
        (insert (format "\n    \"%s\"" file)))
      (insert "\n    ))\n\n")
      (insert ";; To use this list, update the 'acl2files' variable in storage.el\n")
      
      (switch-to-buffer-other-window "*ACL2files-update*")
      (erase-buffer)
      (insert-buffer-substring (current-buffer))
      (goto-char (point-min))
      (message "New acl2files list with %d certified books generated in buffer *ACL2files-update*" 
               (length relative-paths)))))

;; Convenience function to test with a smaller subset first
(defun regenerate-acl2ml-definitions-sample ()
  "Regenerate definitions for a sample of certified books (for testing)"
  (interactive)
  (let* ((acl2-books-dir (or (getenv "ACL2_SYSTEM_BOOKS") "/home/acl2/books"))
         (all-certified-books (get-certified-acl2-books acl2-books-dir))
         ;; Take first 50 books as a sample
         (sample-books (seq-take all-certified-books 1)))
    
    (message "Testing with %d sample books from %s" (length sample-books) acl2-books-dir)
    
    ;; Create new definitions directory if it doesn't exist
    (unless (file-exists-p (concat *home-dir* "definitions/global-sample/"))
      (make-directory (concat *home-dir* "definitions/global-sample/") t))
    
    ;; Process sample books
    (let ((processed 0)
          (errors 0))
      (dolist (book sample-books)
        (message "Processing sample (%d/%d): %s" (1+ processed) (length sample-books) book)
        (condition-case err
            (progn
              (regenerate-book-definitions-sample book acl2-books-dir)
              (setq processed (1+ processed)))
          (error 
           (message "Error processing %s: %s" book err)
           (setq errors (1+ errors)))))
      
      (message "Sample regeneration complete!")
      (message "Processed: %d books, Errors: %d" processed errors)
      (message "Sample definitions in definitions/global-sample/"))))

(defun regenerate-book-definitions-sample (book-path books-base-dir)
  "Process a single ACL2 book for sample testing"
  (let* ((relative-path (file-relative-name book-path books-base-dir))
         (output-name (replace-regexp-in-string "/" "___" 
                                              (file-name-sans-extension relative-path))))
    
    (find-file book-path)
    (setf definitions nil)
    (setf lemmas nil)
    (beginning-of-buffer)
    
    (condition-case err
        (progn
          (extract-tables-recursive)
          (when definitions
            (let ((output-file (concat *home-dir* "definitions/global-sample/" output-name)))
              (with-temp-file output-file
                (insert (format "%s" definitions)))
              (message "  -> Sample: %d definitions to %s" (length definitions) output-name))))
      (error (message "  -> Sample error: %s" err)))
    
    (kill-buffer)))
