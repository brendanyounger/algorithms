(ns algorithms.binary-quadratic-forms
  (require [clojure.math.numeric-tower :as math]))

(defn sqfof [N]
  (let [D (if (= 1 (mod N 4)) (* 2 N) N)
        S (first (exact-integer-sqrt D))]
    )
  )
