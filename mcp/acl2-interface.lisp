;;;; acl2-interface.lisp
;;;; Interface for communicating with ACL2 8.6 via saved_acl2 binary

(in-package #:acl2ml-mcp)

;;; Configuration
(defparameter *acl2-binary-path* "/home/acl2/saved_acl2"
  "Path to the ACL2 saved_acl2 binary")

(defparameter *acl2-books-dir* "/home/acl2/acl2-8.6/books/"
  "Path to ACL2 books directory")

(defparameter *acl2-timeout* 30
  "Timeout in seconds for ACL2 operations")

;;; Process management
(defvar *acl2-process* nil
  "Current ACL2 process")

(defvar *acl2-input-stream* nil
  "Input stream to ACL2 process")

(defvar *acl2-output-stream* nil
  "Output stream from ACL2 process")

(defun start-acl2-process ()
  "Start ACL2 process and return streams"
  (when *acl2-process*
    (stop-acl2-process))

  (multiple-value-bind (process input-stream output-stream error-stream)
      #+sbcl (sb-ext:run-program *acl2-binary-path* nil
                                 :input :stream
                                 :output :stream
                                 :error :stream
                                 :wait nil
                                 :search nil)
      #+ccl (ccl:run-program *acl2-binary-path* nil
                             :input :stream
                             :output :stream
                             :error :stream
                             :wait nil)
      #-(or sbcl ccl) (error "ACL2 interface not implemented for this Lisp")

    (declare (ignore error-stream))
    (setf *acl2-process* process
          *acl2-input-stream* input-stream
          *acl2-output-stream* output-stream)

    ;; Wait for ACL2 to start up
    (wait-for-acl2-prompt)
    process))

(defun stop-acl2-process ()
  "Stop the current ACL2 process"
  (when *acl2-process*
    #+sbcl (when (sb-ext:process-alive-p *acl2-process*)
             (sb-ext:process-close *acl2-process*))
    #+ccl (when (ccl:external-process-status *acl2-process*)
            (ccl:external-process-kill *acl2-process*))
    (setf *acl2-process* nil
          *acl2-input-stream* nil
          *acl2-output-stream* nil)))

(defun wait-for-acl2-prompt (&optional (timeout *acl2-timeout*))
  "Wait for ACL2 prompt, indicating readiness"
  (let ((start-time (get-universal-time))
        (buffer "")
        (prompt-found nil))
    (loop while (and (not prompt-found)
                     (< (- (get-universal-time) start-time) timeout))
          do (when (listen *acl2-output-stream*)
               (let ((char (read-char *acl2-output-stream*)))
                 (setf buffer (concatenate 'string buffer (string char)))
                 ;; Look for ACL2 prompts: "ACL2 !>" or "ACL2 p>"
                 (when (or (search "ACL2 !>" buffer)
                          (search "ACL2 p>" buffer))
                   (setf prompt-found t))))
             (sleep 0.1))
    (unless prompt-found
      (error "Timeout waiting for ACL2 prompt"))
    buffer))

(defun send-to-acl2 (command)
  "Send command to ACL2 and return response"
  (unless *acl2-process*
    (start-acl2-process))

  (format *acl2-input-stream* "~A~%" command)
  (force-output *acl2-input-stream*)

  (wait-for-acl2-prompt))

;;; High-level interface
(defmacro with-acl2-session (&body body)
  "Execute body with an active ACL2 session"
  `(unwind-protect
       (progn
         (unless *acl2-process*
           (start-acl2-process))
         ,@body)
     (stop-acl2-process)))

(defun eval-in-acl2 (form)
  "Evaluate a form in ACL2 and return the result"
  (let ((command (if (stringp form)
                     form
                     (format nil "~S" form))))
    (send-to-acl2 command)))

(defun quit-to-common-lisp ()
  "Switch from ACL2 to Common Lisp mode using :q"
  (send-to-acl2 ":q"))

(defun return-to-acl2 ()
  "Return from Common Lisp to ACL2 using (lp)"
  (send-to-acl2 "(lp)"))

(defun load-acl2-book (book-name)
  "Load an ACL2 book"
  (eval-in-acl2 `(include-book ,book-name)))

(defun certify-acl2-book (book-name)
  "Certify an ACL2 book"
  (eval-in-acl2 `(certify-book ,book-name)))

;;; Utility functions for working with ACL2 data
(defun acl2-symbol-p (obj)
  "Check if object is an ACL2 symbol"
  (and (symbolp obj)
       (not (keywordp obj))))

(defun acl2-package-name (symbol)
  "Get the package name of an ACL2 symbol"
  (when (acl2-symbol-p symbol)
    (package-name (symbol-package symbol))))

(defun normalize-acl2-symbol (symbol)
  "Normalize an ACL2 symbol for indexing"
  (if (acl2-symbol-p symbol)
      (intern (symbol-name symbol) :keyword)
      symbol))

;;; Error handling
(define-condition acl2-error (error)
  ((message :initarg :message :reader acl2-error-message))
  (:report (lambda (condition stream)
             (format stream "ACL2 Error: ~A" (acl2-error-message condition)))))

(defun signal-acl2-error (message &rest args)
  "Signal an ACL2-related error"
  (error 'acl2-error :message (apply #'format nil message args)))