;;;; definitions-index.lisp
;;;; Definitions indexing for ACL2 books - updated for ACL2 8.6

(in-package #:acl2ml-mcp)

;;; Global definitions index storage
(defvar *definitions-index* (make-hash-table :test 'equal)
  "Hash table mapping definition names to their information")

(defvar *definitions-cache* nil
  "Cached list of all definitions")

;;; Definition structure
(defstruct acl2-definition
  name                    ; Symbol name
  type                    ; defun, defthm, defmacro, etc.
  arity                   ; Number of arguments
  body                    ; Definition body
  book                    ; Book/library name
  dependencies           ; List of dependencies
  guards                 ; Guard conditions
  features              ; Extracted features for ML
  )

;;; File path utilities for ACL2 8.6
(defun mangle-book-path (book-path)
  "Convert book path to filesystem-safe name (ACL2 8.6 compatible)"
  (cl-ppcre:regex-replace-all "[/\\.]" book-path "___"))

(defun unmangle-book-path (mangled-path)
  "Convert mangled path back to book path"
  (cl-ppcre:regex-replace-all "___" mangled-path "/"))

(defun get-definitions-file-path (book-path &key (base-dir "../definitions/global/"))
  "Get the path to the definitions file for a book"
  (merge-pathnames (mangle-book-path book-path) base-dir))

;;; S-expression parsing for ACL2 definitions
(defun parse-acl2-definition (def-sexpr book-name)
  "Parse a definition S-expression from the old format"
  ;; Old format: (name type arity body-info...)
  (when (and def-sexpr (listp def-sexpr) (>= (length def-sexpr) 3))
    (let* ((name (first def-sexpr))
           (type (second def-sexpr))
           (arity (third def-sexpr))
           (body (when (> (length def-sexpr) 3) (fourth def-sexpr))))
      (make-acl2-definition
       :name name
       :type type
       :arity arity
       :body body
       :book book-name))))

(defun extract-definition-from-acl2 (def-form)
  "Extract definition info directly from ACL2 form"
  (when (and (listp def-form) (symbolp (first def-form)))
    (let ((def-type (first def-form)))
      (case def-type
        (defun
         (when (>= (length def-form) 4)
           (let ((name (second def-form))
                 (args (third def-form))
                 (body (nthcdr 3 def-form)))
             (make-acl2-definition
              :name name
              :type 'defun
              :arity (length args)
              :body body))))
        (defthm
         (when (>= (length def-form) 3)
           (let ((name (second def-form))
                 (formula (third def-form))
                 (hints (when (> (length def-form) 3) (nthcdr 3 def-form))))
             (make-acl2-definition
              :name name
              :type 'defthm
              :arity 0
              :body (list formula hints)))))
        ((defmacro defconst defstobj)
         (when (>= (length def-form) 3)
           (make-acl2-definition
            :name (second def-form)
            :type def-type
            :body (nthcdr 2 def-form))))))))

;;; Loading definitions from existing index files
(defun load-definitions-from-file (file-path book-name)
  "Load definitions from an existing index file"
  (when (probe-file file-path)
    (with-open-file (stream file-path :direction :input)
      (let ((definitions-list (read stream nil nil)))
        (when definitions-list
          (mapcar (lambda (def-sexpr)
                    (parse-acl2-definition def-sexpr book-name))
                  definitions-list))))))

(defun save-definitions-to-file (definitions file-path)
  "Save definitions to an index file"
  (ensure-directories-exist file-path)
  (with-open-file (stream file-path :direction :output :if-exists :supersede)
    (print (mapcar (lambda (def)
                     (list (acl2-definition-name def)
                           (acl2-definition-type def)
                           (acl2-definition-arity def)
                           (acl2-definition-body def)))
                   definitions)
           stream)))

;;; Index management
(defun load-definitions-index (&key (definitions-dir "../definitions/global/"))
  "Load all definitions from the definitions directory"
  (clrhash *definitions-index*)
  (setf *definitions-cache* nil)

  (let ((definitions-path (pathname definitions-dir)))
    (when (probe-file definitions-path)
      (dolist (file-path (directory (merge-pathnames "*" definitions-path)))
        (let* ((filename (file-namestring file-path))
               (book-name (unmangle-book-path filename))
               (definitions (load-definitions-from-file file-path book-name)))
          (when definitions
            (dolist (def definitions)
              (when def
                (setf (gethash (acl2-definition-name def) *definitions-index*) def)))))))))

(defun find-definition (name)
  "Find a definition by name"
  (gethash name *definitions-index*))

(defun list-definitions (&key (book-filter nil) (type-filter nil))
  "List all definitions, optionally filtered by book or type"
  (let ((results nil))
    (maphash (lambda (name def)
               (declare (ignore name))
               (when (and (or (null book-filter)
                             (string= book-filter (acl2-definition-book def)))
                         (or (null type-filter)
                             (eq type-filter (acl2-definition-type def))))
                 (push def results)))
             *definitions-index*)
    (sort results (lambda (a b)
                    (string< (string (acl2-definition-name a))
                            (string (acl2-definition-name b)))))))

;;; Regeneration from ACL2 books for ACL2 8.6
(defun scan-acl2-book-for-definitions (book-path)
  "Scan an ACL2 book file for definitions using ACL2 8.6"
  (with-acl2-session
    ;; Load the book in ACL2
    (handler-case
        (progn
          (load-acl2-book book-path)
          ;; Switch to Common Lisp to introspect
          (quit-to-common-lisp)

          ;; Use ACL2's world to extract definitions
          (let ((definitions nil))
            ;; This would need to be implemented with proper ACL2 8.6 world access
            ;; For now, return empty list - to be filled in with actual ACL2 introspection
            definitions))
      (error (e)
        (format t "Error scanning book ~A: ~A~%" book-path e)
        nil))))

(defun extract-definitions-from-book-file (book-file-path)
  "Extract definitions by parsing the book file directly"
  (when (probe-file book-file-path)
    (with-open-file (stream book-file-path :direction :input)
      (let ((definitions nil)
            (book-name (pathname-name book-file-path)))
        (loop for form = (read stream nil :eof)
              until (eq form :eof)
              do (let ((def (extract-definition-from-acl2 form)))
                   (when def
                     (setf (acl2-definition-book def) book-name)
                     (push def definitions))))
        (nreverse definitions)))))

(defun regenerate-definitions-for-book (book-path)
  "Regenerate definitions index for a single book"
  (let* ((full-path (merge-pathnames book-path *acl2-books-dir*))
         (lisp-file (make-pathname :type "lisp" :defaults full-path))
         (definitions (extract-definitions-from-book-file lisp-file))
         (index-file (get-definitions-file-path book-path)))

    (when definitions
      (save-definitions-to-file definitions index-file)
      (dolist (def definitions)
        (setf (gethash (acl2-definition-name def) *definitions-index*) def))
      (format t "Indexed ~A definitions from ~A~%" (length definitions) book-path))
    definitions))

(defun regenerate-definitions-index (&key (books-list nil) (scan-all nil))
  "Regenerate the definitions index for ACL2 8.6"
  (cond
    (scan-all
     ;; Scan all .lisp files in books directory
     (let ((book-files (directory (merge-pathnames "**/*.lisp" *acl2-books-dir*))))
       (dolist (book-file book-files)
         (let ((relative-path (enough-namestring book-file *acl2-books-dir*)))
           (regenerate-definitions-for-book relative-path)))))

    (books-list
     ;; Regenerate for specific books
     (dolist (book-path books-list)
       (regenerate-definitions-for-book book-path)))

    (t
     ;; Default: regenerate common system books
     (let ((system-books '("arithmetic/top"
                          "ihs/ihs-definitions"
                          "std/util/define"
                          "std/lists/top"
                          "centaur/fty/deftypes")))
       (dolist (book system-books)
         (regenerate-definitions-for-book book))))))

;;; Search and query functions
(defun search-definitions (pattern &key (field :name))
  "Search definitions by pattern in specified field"
  (let ((results nil))
    (maphash (lambda (name def)
               (declare (ignore name))
               (let ((value (case field
                             (:name (string (acl2-definition-name def)))
                             (:book (acl2-definition-book def))
                             (:type (string (acl2-definition-type def)))
                             (t ""))))
                 (when (search pattern value :test #'char-equal)
                   (push def results))))
             *definitions-index*)
    results))

(defun get-definitions-by-book (book-name)
  "Get all definitions from a specific book"
  (list-definitions :book-filter book-name))

(defun get-definitions-by-type (def-type)
  "Get all definitions of a specific type (defun, defthm, etc.)"
  (list-definitions :type-filter def-type))