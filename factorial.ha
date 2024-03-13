(defn fibonacci*
  (n1 n2 iter how-many)
  (if (eq iter how-many)
    nil
    (do
      (let iter-th-fib (add n1 n2))
      (prln iter-th-fib)
      (let next-iter (inc iter))
      (fibonacci* n2 iter-th-fib next-iter how-many))))

(defn fibonacci
  (how-many)
  (fibonacci* 0 1 0 how-many))

(def how-many 10)
(fibonacci how-many)

(defn testing-auto-pop (x)
  (if false (do 2 3 4 (let a 5) a)
           2))

(prln (testing-auto-pop 2))