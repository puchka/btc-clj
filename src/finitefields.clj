(ns finitefields)

(defrecord FieldElement [e p])

(defn modpow [b e m]
  (mod (reduce #(mod (* %1 %2) m) (repeat e b)) m))

(defn fermat-test
  "Integer -> Boolean"
  [p]
  (let [a (inc (rand-int (dec p)))]
    (= (modpow a p p) a)))

(defn prime?
  "Integer -> Boolean"
  [p]
  (every? true? (take 50 (repeatedly #(fermat-test p)))))

(defn make-field-element
  "Integer Integer -> FieldElement"
  [e p]
  (if (and (<= 0 e) (< e p) (prime? p))
    (FieldElement. e p)
    (println "Invalid input")))

(defprotocol FieldOperations
  (+f    [x y])
  (-f    [x y])
  (*f    [x y])
  (divf  [x y])
  (**f   [x k]))

(defn assert= [p p2]
  (assert (= p p2) "Fields need to be of the same prime order"))

(extend-type FieldElement
  FieldOperations
  (+f [{e :e p :p} {e2 :e p2 :p}]
    (assert= p p2)
    (FieldElement. (mod (+ e e2) p) p))
  (-f [{e :e p :p} {e2 :e p2 :p}]
    (assert= p p2)
    (FieldElement. (mod (- e e2) p) p))
  (*f [{e :e p :p} {e2 :e p2 :p}]
    (assert= p p2)
    (make-field-element (mod (* e e2) p) p))
  (**f [{e :e p :p} k]
    (make-field-element (mod (Math/pow e k) p) p))
  )
