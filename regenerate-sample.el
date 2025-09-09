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
