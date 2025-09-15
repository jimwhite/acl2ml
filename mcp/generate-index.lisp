;; Generate definitions index from ACL2 8.6 books
;; Run with: /home/acl2/saved_acl2 < generate-index.lisp

;; Start in ACL2 mode, switch to Common Lisp
:q

(load "~/quicklisp/setup.lisp")
(ql:quickload :cl-ppcre :silent t)

(format t "~%=== ACL2 Definitions Index Generator ===~%")

(defpackage #:acl2ml-indexer
  (:use #:cl #:cl-ppcre))

(in-package :acl2ml-indexer)

(defparameter *acl2-books-path* "/home/acl2/books/")
(defparameter *index-output-path* "/workspaces/acl2ml/mcp/definitions-index.dat")

(defstruct acl2-definition
  name type file line body complexity)

(defparameter *definitions* (make-hash-table :test 'equal))
(defparameter *files-processed* 0)
(defparameter *definitions-found* 0)

(defun extract-definition-info (form file-path line-num)
  "Extract info from ACL2 definition form"
  (when (and (listp form) (>= (length form) 2))
    (let ((def-type (first form))
          (def-name (second form)))
      (when (member def-type '(defun defthm defmacro defconst))
        (let* ((body (if (eq def-type 'defun)
                        (fourth form)  ; function body
                        (third form))) ; theorem body
               (complexity (length (format nil "~S" body))))
          (make-acl2-definition
           :name def-name
           :type def-type
           :file file-path
           :line line-num
           :body body
           :complexity complexity))))))

(defun index-acl2-file (file-path)
  "Index all definitions in an ACL2 file"
  (handler-case
      (with-open-file (stream file-path :direction :input)
        (loop with line-num = 1
              for form = (read stream nil :eof)
              until (eq form :eof)
              do (let ((def (extract-definition-info form file-path line-num)))
                   (when def
                     (setf (gethash (acl2-definition-name def) *definitions*) def)
                     (incf *definitions-found*)))
                 (incf line-num)))
    (error (e)
      (format t "Warning: Error reading ~A: ~A~%" file-path e))))

(defun find-lisp-files (directory)
  "Find all .lisp files in directory tree"
  (let ((files nil))
    (labels ((walk-directory (dir)
               (when (probe-file dir)
                 (dolist (entry (directory (merge-pathnames "*.*" dir)))
                   (cond
                     ((and (pathname-name entry) (not (pathname-type entry)))
                      ;; Directory - recurse
                      (walk-directory entry))
                     ((and (pathname-name entry)
                           (string= (pathname-type entry) "lisp"))
                      (push entry files)))))))
      (walk-directory (pathname directory)))
    files))

(defun save-index ()
  "Save definitions index to file"
  (with-open-file (stream *index-output-path*
                          :direction :output
                          :if-exists :supersede)
    (format stream ";; ACL2 Definitions Index~%")
    (format stream ";; Generated: ~A~%" (get-universal-time))
    (format stream ";; Files processed: ~A~%" *files-processed*)
    (format stream ";; Definitions found: ~A~%~%" *definitions-found*)

    (maphash (lambda (name def)
               (format stream "(~S ~S ~S ~A ~A)~%"
                       (acl2-definition-name def)
                       (acl2-definition-type def)
                       (acl2-definition-file def)
                       (acl2-definition-line def)
                       (acl2-definition-complexity def)))
             *definitions*)))

(format t "Scanning ACL2 books directory: ~A~%" *acl2-books-path*)

(let ((lisp-files (find-lisp-files *acl2-books-path*)))
  (format t "Found ~A .lisp files~%" (length lisp-files))

  ;; Process first 100 files for testing
  (dolist (file (subseq lisp-files 0 (min 100 (length lisp-files))))
    (incf *files-processed*)
    (when (zerop (mod *files-processed* 10))
      (format t "Processed ~A files, found ~A definitions~%"
              *files-processed* *definitions-found*))
    (index-acl2-file file)))

(format t "~%Indexing complete:~%")
(format t "Files processed: ~A~%" *files-processed*)
(format t "Definitions found: ~A~%" *definitions-found*)

(save-index)
(format t "Index saved to: ~A~%" *index-output-path*)

(sb-ext:exit)