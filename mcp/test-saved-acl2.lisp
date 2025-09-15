;;;; test-saved-acl2.lisp
;;;; Test saved_acl2 binary and :q/:LP workflow

;; This file should be run with: /home/acl2/saved_acl2 < test-saved-acl2.lisp

;; First, we're in ACL2 mode
;; Test basic ACL2 functionality
(+ 1 2 3)

;; Load a simple book to test
:ubt 1

;; Now switch to Common Lisp mode
:q

;; We're now in Common Lisp - test some CL functionality
(format t "~%=== Now in Common Lisp mode ===~%")
(format t "Testing CL functionality...~%")

;; Test our feature extraction in CL mode
(defun simple-extract-features (expr)
  "Simple feature extraction in CL mode"
  (labels ((count-atoms (e)
             (if (atom e)
                 1
                 (reduce #'+ (mapcar #'count-atoms e) :initial-value 0))))
    (let ((atom-count (count-atoms expr))
          (max-depth (labels ((depth (e level)
                               (if (atom e)
                                   level
                                   (reduce #'max (mapcar (lambda (x) (depth x (1+ level))) e)
                                           :initial-value level))))
                      (depth expr 0))))
      (list :atoms atom-count :depth max-depth :expr expr))))

;; Test it
(let ((features (simple-extract-features '(implies (consp x) (equal x x)))))
  (format t "✓ Feature extraction: ~A~%" features))

;; Test accessing some ACL2 world information (if available)
(format t "Testing ACL2 integration from CL mode...~%")

;; Return to ACL2
(format t "Returning to ACL2 mode...~%")
(lp)

;; We're back in ACL2 - test some more ACL2 functionality
(+ 4 5 6)

;; Test a simple theorem
(thm (equal (+ x y) (+ y x)))

;; Exit
:q
(sb-ext:exit)