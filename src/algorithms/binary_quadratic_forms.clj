(ns algorithms.binary-quadratic-forms
  (require [clojure.math.numeric-tower :as math]))

(def floor-sqrt
  (memoize
    (fn [N]
      (first (math/exact-integer-sqrt N)))))

(defn cf-next [[bi N n d]]
  (let [n'  (/ (- N (* d d)) n)
        bi' (math/floor (/ (+ (floor-sqrt N) d) n'))
        d'  (- (* bi' n') d)]
    [bi' N n' d']))

;; form: Qi / (sqrt(N) - Pi)
(defn cf-iter [[b N P Q]]
  (let [Qp1 (/ (- N (* P P)) Q)
        bp1 (math/floor (/ (+ (floor-sqrt N) P) Qp1))
        Pp1 (- (* bp1 Qp1) P)]
    [bp1 N Pp1 Qp1]))

(defn sqrt-cf [N]
  (map first (iterate cf-iter [(floor-sqrt N) N (floor-sqrt N) 1])))

(defn sqrt-cf-alt [N]
  (let [sqrtN (floor-sqrt N)
        oddSqrtN (if (= 0 (mod sqrtN 2)) (- sqrtN 1) sqrtN)]
  (map first (iterate cf-iter [(floor-sqrt N) N oddSqrtN 2]))))

(defn convergent-iter [[bs Am1 Bm1 Am2 Bm2]]
  (let [b (first bs)
        A (+ (* b Am1) Am2)
        B (+ (* b Bm1) Bm2)]
    [(rest bs) A B Am1 Bm1]))

(defn convergents [bs]
  (rest (map (fn [[bs A B Am1 Bm1]] [A B]) (iterate convergent-iter [bs 1 0 0 1]))))

(defn ratios [cs]
  (map (fn [[n d]] (float (/ n d))) cs))

;; (require '[algorithms.binary-quadratic-forms :as bqf] :reload)
;; (take 10 (bqf/ratios (bqf/convergents (bqf/sqrt-cf 41))))
