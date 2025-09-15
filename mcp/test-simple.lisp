(in-package "ACL2")

(defun simple-append (x y)
  (if (endp x)
      y
      (cons (car x) (simple-append (cdr x) y))))

(defun simple-length (x)
  (if (endp x)
      0
      (+ 1 (simple-length (cdr x)))))

(defthm simple-append-nil
  (implies (true-listp x)
           (equal (simple-append x nil) x)))

(defthm simple-append-assoc
  (equal (simple-append (simple-append x y) z)
         (simple-append x (simple-append y z))))