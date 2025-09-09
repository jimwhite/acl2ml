(in-package "ACL2")

(include-book "arithmetic-5/top" :dir :system)

;; Sum

(Defun theta_sum (n)
  (if (zp n)
      0
    (+ n (theta_sum (- n 1)))))


(defun helper_sum (n a)
  (if (zp n)
      a
      (helper_sum (- n 1) (+ n a))))

(defun fn_sum (n) (helper_sum n 0))

(defthm helper_is_theta_sum
  (implies (and (natp n)
                (natp a))
           (equal (helper_sum n a)
                  (+ a (theta_sum n)))))

(defthm fn_is_theta_sum
  (implies (natp n)
           (equal (fn_sum n)
                  (theta_sum n))))

;; Factorial 

(defun theta_fact (n)
  (if (zp n)
      1
      (* n (theta_fact (- n 1)))))

(defun helper_fact (n a)
  (if (zp n)
      a
      (helper_fact (- n 1) (* a n))))

(defun fn_fact (n) (helper_fact n 1))


(defthm helper_is_theta_fact
  (implies (and (natp n)
		(natp a))
	   (equal (helper_fact n a)
		  (* a (theta_fact n)))))

(defthm fn_is_theta_fact
  (implies (natp n)
           (equal (fn_fact n)
                  (theta_fact n))))

;; Expt

(defun theta_expt (n m)
  (expt n m))

(defun helper_expt (n m a)
  (if (zp m)
      a
      (helper_expt n (- m 1) (* n a))))

(defun fn_expt (n m) (helper_expt n m 1))

(defthm helper_is_theta_expt
  (implies (and (natp n)
		(natp m)
                (natp a))
           (equal (helper_expt n m a)
                  (* a (theta_expt n m)))))

(defthm fn_is_theta_expt
  (implies (and (natp n)
		(natp m))
           (equal (fn_expt n m)
                  (theta_expt n m))))

;; Less


(defun theta_less (x y)
  (if (< x y) 1 0))

(defun helper_less (x y)
  (cond ((zp y) 0)
        ((zp x) 1)
        (t (helper_less (- x 1) (- y 1)))))

(defun fn_less (x y) (helper_less x y))

(defthm helper_is_theta_less
  (implies (and (natp x)
                (natp y))
           (equal (helper_less x y)
                  (theta_less x y))))

(defthm fn_is_theta_less
  (implies (and (natp x)
		(natp y))
           (equal (fn_less x y)
                  (theta_less x y))))


;; Multiplication 

(defun theta_mul (x y)
  (* x y))

(defun helper_mul (x y a)
  (if (zp y)
      a
    (helper_mul x (- y 1) (+ x a))))

(defun fn_mul (x y)
  (helper_mul x y 0))


(defthm helper_is_theta_mul
  (implies (and (natp n)
		(natp m)
                (natp a))
           (equal (helper_mul n m a)
                  (+ a (theta_mul n m)))))

(defthm fn_is_theta_mul
  (implies (and (natp n)
		(natp m))
           (equal (fn_mul n m)
                  (theta_mul n m))))


;; Power 

(defun theta_power (n)
  (expt 2 n))


(defun helper_power (n a)
  (if (zp n)
      a
      (helper_power (- n 1) (+ a a))))

(defun fn_power (n) (helper_power n 1))


(defthm helper_is_theta_power
  (implies (and (natp n)
                (natp a))
           (equal (helper_power n a)
                  (* a (theta_power n)))))

(defthm fn_is_theta_power
  (implies (natp n)
           (equal (fn_power n)
                  (theta_power n))))


;; Sum square

(defun theta_sum_square (n)
  (if (zp n)
      0
    (+ (* n n) (theta_sum_square (- n 1)))))


(defun helper_sum_square (n a)
  (if (zp n)
      a
      (helper_sum_square (- n 1) (+ (* n n) a))))

(defun fn_sum_square (n) (helper_sum_square n 0))

(defthm helper_is_theta_sum_square
  (implies (and (natp n)
                (natp a))
           (equal (helper_sum_square n a)
                  (+ a (theta_sum_square n)))))

(defthm fn_is_theta_sum_square
  (implies (natp n)
           (equal (fn_sum_square n)
                  (theta_sum_square n))))







;; Fibonacci

(defun theta_fib (n)
  (if (zp n)
      0
    (if (equal n 1)
        1
      (+ (theta_fib (- n 1))
         (theta_fib (- n 2))))))

(defun helper_fib (n j k)
  (if (zp n)
      j
    (if (equal n 1)
        k
    (helper_fib (- n 1) k (+ j k)))))

(defun fn_fib (n) (helper_fib n 0 1))






