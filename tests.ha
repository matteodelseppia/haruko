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


(defn say-hello
  ()
  (prln "Welcome to haruko-lang!"))


(defn return-1
  ()
  1)

(defn test-compose
  ()
  (-> (return-1)
      (inc)
      (dec)
      (power 3)
      (inc)
      (pow 0)
      (add 5)
      (sum-of-squares)
      (cat " = 91")
      (prln)))


(defn test-nested
  ()
  (do
     (let x 2)
     (let y 3)
     (if (gt x y)
         (do
            (let z 4)
            (let w 5)
            (add z w))
         (do
            (let z 5)
            (let w 6)
            (pow z w)))))


(defn return-123-list
  ()
  (list$ 1 2 3 4 5 6))

(defn return-1357-list
  ()
  (list$ 1 3 5 7 9 11))

(defn merge
  (lst1 lst2 result)
  (do
    (if (empty lst1)
        (app result lst2)
        (if (empty lst2)
            (app result lst1)
            (do
               (let head1 (head lst1))
               (let head2 (head lst2))
               (if (lt head1 head2)
                   (merge (tail lst1) lst2 (app result head1))
                   (merge lst1 (tail lst2) (app result head2))))))))

(defn middle
  (lst)
  (-> lst (len) (div 2)))

(defn merge-sort
  (lst)
  (if (or (empty lst) (eq 1 (len lst)))
      lst
      (do
        (let half-index (middle lst))
        (let left (slice lst 0 half-index))
        (let right (slice lst half-index (len lst)))
        (let res-left (merge-sort left))
        (let res-right (merge-sort right))
        (merge res-left res-right (list$)))))

(say-hello)

(prln (cat "GCD of 147, 105: " (gcd 147 105)))

(prln (cat "10th Fibonacci: " (fib 10)))

(prln (cat "Factorial of 5: " (factorial 5)))

(prln (cat "2^5: " (power 2 5)))

(prln (cat "Sum of squares up to 5: " (sum-of-squares 5)))

(prln (cat "PI^3: " (power 3.1415926535 3)))

(test-compose)

(prln$ "should print 15625:" (-> (test-nested)))


(def lst (list$ 43 1 34 3 23 4 24 5))

(prln$ "List before merge sort: " lst)
(prln$ "List after merge sort: " (merge-sort lst))
