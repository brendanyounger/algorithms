(ns algorithms.core)

(defn roughly
  "With two arguments, accepts a value within delta of the
   expected value. With one argument, the delta is 1/1000th
   of the expected value."
  ([actual expected delta]
    (and (number? actual)
         (>= expected (- actual delta))
         (<= expected (+ actual delta))))
  ([actual expected]
    (roughly actual expected (Math/abs (* 0.001 expected)))))
