(ns ellipticcurves)

(defrecord Point [x y a b])

(defn **
  "Bit integer exponentiation"
  [b e]
  (reduce * (repeat (bigint e) (bigint b))))

(defn on-curve?
  "Checks if point is on elliptic curve"
  [x y a b]
  (= (int (** y 2)) (int (+ (** x 3) (* a x) b))))

(defn make-pt
  "Constructor function for elliptic curve points with validations"
  [x y a b]
  (if (and (= x ##Inf) (= y ##Inf))
    (Point. x y a b)
    (do
      (assert (on-curve? x y a b))
      (Point. x y a b))))

(defprotocol PointOps
  (+p [x y]))

(defn slope
  "Calculates slope of a line"
  [x1 x2 y1 y2]
  (int (/ (- y2 y1) (- x2 x1))))

(defn tangent-slope
  "Calcutes the slope of a tangent line to the elliptic curve"
  [x y a]
  (int (/ (+ (* 3 (** x 2)) a) (* 2 y))))

(extend-type Point
  PointOps
  (+p [{x1 :x y1 :y a1 :a b1 :b, :as p1}
       {x2 :x y2 :y a2 :a b2 :b, :as p2}]
    (assert (and (= a1 a2) (= b1 b2)) "Points aren't on the same curve.")
    (cond
      (= x1 ##Inf) p2
      (= x2 ##Inf) p1
      (and (= x1 x2) (not= y1 y2)) (make-pt ##Inf ##Inf a1 b1)
      (not= x1 x2) (let [s (slope x1 x2 y1 y2)
                         x3 (- (int (** s 2)) x1 x2)
                         y3 (- (* s (- x1 x3)) y1)]
                     (make-pt x3 y3 a1 b1))
      (and (= x1 x2) (= y1 y2)) (let [s (tangent-slope x1 y1 a1)
                                      x3 (- (int (** s 2)) x1 x2)
                                      y3 (- (* s (- x1 x3)) y1)]
                                  (make-pt x3 y3 a1 b1)))))
