(def a 10)

(defn factorial (x)
  (if (eq x 0)
     1
     (mul x (factorial (dec x)))))

(def b (inc a))

(if (gt (factorial a) (factorial b))
  (prln "a is greater")
  (prln "b is greater")) 