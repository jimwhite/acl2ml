;; Test ACL2 definitions for extraction testing

(defun factorial (n)
  (if (or (not (integerp n)) (<= n 0))
      1
      (* n (factorial (- n 1)))))

(defun append-lists (x y)
  (if (endp x)
      y
      (cons (car x) (append-lists (cdr x) y))))

(defthm factorial-positive
  (implies (and (integerp n) (> n 0))
           (> (factorial n) 0)))

(defun member-equal (x lst)
  (if (endp lst)
      nil
      (if (equal x (car lst))
          t
          (member-equal x (cdr lst)))))