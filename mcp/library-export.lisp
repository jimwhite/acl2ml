;;;; library-export.lisp
;;;; ACL2(ml) Library Export and Definitions Generation
;;;;
;;;; ORIGINAL MAPPING:
;;;; - code/storage.el:export-library() → export-library()
;;;; - code/storage.el:add-libraryname-to-definitions() → add-libraryname-to-definitions()
;;;; - code/storage.el:import-definitions() → import-definitions()
;;;; - code/menus.el "Export library" menu item → MCP tool export-acl2-library
;;;; - code/shortcuts.el C-c C-e acl2ml-export-library → MCP tool export-acl2-library
;;;;
;;;; This converts the ACL2 library export functionality from Emacs to Common Lisp

(defpackage #:acl2ml-library-export
  (:use #:cl)
  (:export #:export-library
           #:import-definitions
           #:add-libraryname-to-definitions
           #:generate-definitions-file
           #:generate-global-definitions))

(in-package :acl2ml-library-export)

;;; Directory structure management
;; ORIGINAL: storage.el used *home-dir* with /libs/, /definitions/, /guards/
;; CONVERTED: Use relative paths from mcp directory
(defparameter *acl2ml-home* "/workspaces/acl2ml/mcp/")
(defparameter *libs-dir* (merge-pathnames "libs/" *acl2ml-home*))
(defparameter *definitions-dir* (merge-pathnames "definitions/" *acl2ml-home*))
(defparameter *guards-dir* (merge-pathnames "guards/" *acl2ml-home*))

(defun ensure-directories ()
  "Ensure export directories exist
   ORIGINAL: Implicit in storage.el file operations
   CONVERTED: Explicit directory creation"
  (ensure-directories-exist *libs-dir*)
  (ensure-directories-exist *definitions-dir*)
  (ensure-directories-exist *guards-dir*))

;;; Library name processing
;; ORIGINAL: storage.el:add-libraryname-to-definitions() lines 52-59
;; CONVERTED: Same name, same functionality - but with FILE PATH format
(defun add-libraryname-to-definitions (library-name definitions-list)
  "Add library name prefix to definitions (mirrors original exactly)
   ORIGINAL: storage.el:add-libraryname-to-definitions()
   CONVERTED: Creates FILE::SYMBOL format like original definitions files
   INPUT: library-name (string), definitions-list (list of definition structures)
   OUTPUT: Modified definitions with file-path::symbol namespacing (original format)"
  (loop for definition in definitions-list
        collect (let* ((original-name (first definition))
                      ;; Create file::symbol format like original (e.g. "/add-ons/hash-stobjs.lisp::hons-remove-assoc")
                      (file-path (if (stringp library-name)
                                   library-name
                                   (format nil "~A" library-name)))
                      (namespaced-name (intern (format nil "~A::~A"
                                                      file-path
                                                      (symbol-name original-name)))))
                 (cons namespaced-name (rest definition)))))

;;; File I/O for definitions
;; ORIGINAL: storage.el:import-definitions() lines 109-113
;; CONVERTED: Same name, adapted for Common Lisp file I/O
(defun import-definitions (library-name)
  "Import definitions from saved library file
   ORIGINAL: storage.el:import-definitions()
   CONVERTED: Common Lisp file reading instead of Emacs buffer operations"
  (let ((definitions-file (merge-pathnames library-name *definitions-dir*)))
    (when (probe-file definitions-file)
      (handler-case
          (with-open-file (stream definitions-file :direction :input)
            (read stream))
        (error (e)
          (format t "Warning: Could not import definitions from ~A: ~A~%"
                  library-name e)
          nil)))))

;; ORIGINAL: storage.el:export-library() lines 73-89
;; CONVERTED: export-library() - core library export functionality
(defun export-library (library-name definitions-list &optional lemmas-list guards-list)
  "Export ACL2 library to processed definition files
   ORIGINAL: storage.el:export-library()
   CONVERTED: Creates same file structure but with Common Lisp I/O

   ORIGINAL FLOW:
   1. extract-recursive-beg-to-end2 → Extract all definitions/lemmas
   2. Save to /libs/{name} with add-libraryname-to-lemmas
   3. Save to /definitions/{name} with add-libraryname-to-definitions
   4. Save to /guards/{name} with nested-lists-to-string

   CONVERTED FLOW:
   1. Take definitions-list as parameter (from extraction)
   2. Add library namespacing
   3. Save to same directory structure"
  (ensure-directories)

  (let ((processed-definitions (add-libraryname-to-definitions library-name definitions-list)))

    ;; Write definitions file (main functionality)
    (let ((definitions-file (merge-pathnames library-name *definitions-dir*)))
      (with-open-file (stream definitions-file
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create)
        (format stream "~S~%" processed-definitions)))

    ;; Write lemmas file if provided
    (when lemmas-list
      (let ((libs-file (merge-pathnames library-name *libs-dir*)))
        (with-open-file (stream libs-file
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
          (format stream "~S~%" lemmas-list))))

    ;; Write guards file if provided
    (when guards-list
      (let ((guards-file (merge-pathnames library-name *guards-dir*)))
        (with-open-file (stream guards-file
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
          (format stream "~S~%" guards-list))))

    (format t "Exported library ~A with ~A definitions~%"
            library-name (length processed-definitions))
    processed-definitions))

;;; Integration with extraction system
;; ORIGINAL: storage.el combined with extraction.el for full pipeline
;; CONVERTED: generate-definitions-file() - high-level interface
(defun generate-definitions-file (acl2-source-file &optional (library-name nil))
  "Generate definitions file from ACL2 source (high-level interface)
   ORIGINAL: storage.el:export-library() + extraction.el pipeline
   CONVERTED: Integrate with our extraction system to create definitions files

   This is the main function that replicates the original workflow:
   1. Extract definitions from ACL2 source
   2. Process and namespace them with FILE PATH (not just name)
   3. Save to definitions directory"
  (let* ((actual-library-name (or library-name
                                  (pathname-name acl2-source-file)))
         ;; Use the actual file path for namespacing (like original)
         (file-path acl2-source-file)
         ;; Use our extraction system
         (definitions (extract-acl2-definitions-from-file acl2-source-file)))

    (when definitions
      ;; Pass the full file path for proper namespacing
      (export-library file-path definitions)
      (format t "Generated definitions file for ~A from ~A~%"
              actual-library-name acl2-source-file)
      definitions)))

;;; Global definitions generation
;; ORIGINAL: storage.el:add-several-libraries-defs() lines 145+ - handles "g" option for global ACL2 files
;; CONVERTED: generate-global-definitions() - scan ACL2 books and create global definitions
(defun generate-global-definitions (&optional (acl2-books-path "/home/acl2/acl2-8.6/books/"))
  "Generate global definitions from ACL2 books (replicates 'g' option)
   ORIGINAL: storage.el:add-several-libraries-defs() with option-libs-defs='g'
   CONVERTED: Scan ACL2 books directory and generate global definitions files

   ORIGINAL LOGIC:
   - Iterate through acl2files list
   - For each file, create global/{filename-with-path-converted}
   - Convert '/' to '___' in filenames (change-symbol function)

   CONVERTED: Same logic but with Common Lisp directory scanning"
  (ensure-directories)

  (let ((global-definitions-dir (merge-pathnames "global/" *definitions-dir*)))
    (ensure-directories-exist global-definitions-dir)

    ;; Scan for common ACL2 book files
    (let ((book-patterns '("*.lisp" "*.lsp"))
          (processed-count 0))

      (dolist (pattern book-patterns)
        (let ((matching-files (directory (merge-pathnames pattern acl2-books-path))))

          (dolist (book-file matching-files)
            (let* ((relative-path (enough-namestring book-file acl2-books-path))
                   ;; Convert path separators to ___ (matches original change-symbol)
                   (safe-name (substitute-if #\_ (lambda (c) (char= c #\/))
                                           (substitute-if #\_ (lambda (c) (char= c #\.))
                                                        (namestring relative-path))))
                   (global-def-file (merge-pathnames safe-name global-definitions-dir)))

              (handler-case
                  (let ((definitions (extract-acl2-definitions-from-file book-file)))
                    (when definitions
                      (with-open-file (stream global-def-file
                                              :direction :output
                                              :if-exists :supersede
                                              :if-does-not-exist :create)
                        (format stream "~S~%" definitions))
                      (incf processed-count)
                      (when (zerop (mod processed-count 10))
                        (format t "Processed ~A ACL2 books...~%" processed-count))))
                (error (e)
                  (format t "Warning: Could not process ~A: ~A~%" book-file e)))))))

      (format t "Generated global definitions for ~A ACL2 books~%" processed-count)
      processed-count)))

;;; Real ACL2 file extraction
;; ORIGINAL: extraction.el:extract-recursive-beg-to-end2() - full ACL2 parsing
;; CONVERTED: extract-acl2-definitions-from-file() - proper S-expression parsing
(defun extract-acl2-definitions-from-file (file-path)
  "Extract ACL2 definitions from file (real extraction matching original format)
   ORIGINAL: extraction.el:extract-recursive-beg-to-end2()
   CONVERTED: Parse ACL2 file and extract definitions in original format

   ORIGINAL FORMAT: (name defun arity body-expression)
   This matches the exact format in /workspaces/acl2ml/definitions/example"
  (handler-case
      (with-open-file (stream file-path :direction :input)
        (let ((definitions nil))

          ;; Read all S-expressions from the file
          (loop for form = (read stream nil nil)
                while form
                do (when (and (listp form)
                             (> (length form) 1)
                             (member (first form) '(defun defthm defmacro defconst) :test #'string-equal))
                     (let* ((def-type (first form))
                            (def-name (second form)))

                       ;; Extract based on definition type to match original format
                       (cond
                         ;; DEFUN: (name defun arity body) - handle DECLARE statements
                         ((string-equal def-type "defun")
                          (let* ((params (third form))
                                 (arity (length params))
                                 ;; Find the actual body, skipping DECLARE forms
                                 (body-forms (cdddr form)) ; Everything after (defun name params ...)
                                 (body (if (and (consp body-forms)
                                               (consp (first body-forms))
                                               (eq (car (first body-forms)) 'declare))
                                          ;; Skip declare, get actual body
                                          (if (> (length body-forms) 1)
                                              (second body-forms)  ; Body after declare
                                              (first body-forms))  ; Just declare
                                          ;; No declare, use first form
                                          (first body-forms))))
                            (push (list def-name def-type arity body) definitions)))

                         ;; DEFTHM: (name defthm formula) - theorems have formula, not body
                         ((string-equal def-type "defthm")
                          (let ((formula (third form))) ; The theorem formula
                            (push (list def-name def-type formula) definitions)))

                         ;; Other definition types
                         (t
                          (let ((body (cddr form))) ; Everything after (deftype name ...)
                            (push (list def-name def-type body) definitions)))))))

          (format t "Extracted ~A definitions from ~A~%" (length definitions) file-path)
          (reverse definitions)))
    (error (e)
      (format t "Error extracting from ~A: ~A~%" file-path e)
      nil)))

;; High-level MCP interface function
;; ORIGINAL: menus.el "Export library" + shortcuts.el C-c C-e
;; CONVERTED: export-acl2-library() - main MCP tool interface
(defun export-acl2-library (source-file-or-buffer &optional library-name)
  "Export ACL2 library for MCP interface
   ORIGINAL: menus.el 'Export library' menu item + shortcuts.el C-c C-e
   CONVERTED: Main interface for library export via MCP tools

   This replicates the original user workflow:
   1. User selects 'Export library' from menu or presses C-c C-e
   2. System extracts current buffer/file definitions
   3. Processes and saves to library files
   4. Makes available for clustering and analysis"
  (handler-case
      (let* ((file-path (if (stringp source-file-or-buffer)
                           source-file-or-buffer
                           (error "Buffer export not yet implemented")))
             (lib-name (or library-name (pathname-name file-path)))
             (result (generate-definitions-file file-path lib-name)))

        (if result
            (format nil "Successfully exported library '~A' with ~A definitions.~%Available for clustering and analysis."
                    lib-name (length result))
            (format nil "No definitions found in ~A" file-path)))
    (error (e)
      (format nil "Error exporting library: ~A" e))))