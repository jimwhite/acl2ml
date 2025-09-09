(include-book "all")

(defthm helper_is_theta_fib
  (equal (helper_fib n j k)
                  (+ (* (theta_fib (- n 1)) j)
                     (* (theta_fib n) k)))
 )