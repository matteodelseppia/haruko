(defn gcd 
  (a b)
  (if (lt a b)
      (gcd b a)
      (do 
        (let rem (mod a b))
        (if (neq rem 0)
            (gcd b rem)
             b ))))

(defn fib
  (n)
  (if (lt n 2)
      n
      (add (fib (dec n)) (fib (sub n 2)))))


(defn factorial
  (n)
  (if (eq n 0)
      1
      (mul n (factorial (dec n)))))


(defn power
  (base exponent)
  (if (eq exponent 0)
      1
      (mul base (power base (dec exponent)))))


(defn sum-of-squares
  (n)
  (if (eq n 0)
      0
      (add (power n 2) (sum-of-squares (sub n 1)))))


(prln (cat "GCD of 147, 105: " (gcd 147 105)))

(prln (cat "10th Fibonacci: " (fib 10)))

(prln (cat "Factorial of 5: " (factorial 5)))

(prln (cat "2^5: " (power 2 5)))

(prln (cat "Sum of squares up to 5: " (sum-of-squares 5)))

(prln (cat "PI^3: " (power 3.1415926535 3)))