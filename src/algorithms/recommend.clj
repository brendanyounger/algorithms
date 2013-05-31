(ns algorithms.recommend
  (require [clojure.set :refer [intersection union difference]]))

(defn- square [x] (* x x))
(defn- vals-for-keys [m ks] (map #(m %) ks))

;; TODO: use matrix-api eventually

(defn euclidean-similarity
  "reciprocal euclidean distance between two maps of keys->floats"
  [a b]
  { :pre [(map? a) (map? b)] }
  (let [common-keys (intersection (set (keys a)) (set (keys b)))]
    (/ 1
      (+ 1
        ;; TODO: a sqrt here?
        (reduce +
          (map square
            (map - (vals-for-keys a common-keys) (vals-for-keys b common-keys))))))))

;; TODO: can calculate the correlation in a streaming manner
(defn- pearson-correlation [x y]
  { :pre [(= (count x) (count y))] }
  (let [n  (count x)
        sx (reduce + x)
        sy (reduce + y)
        sxy (reduce + (map * x y))
        sxx (reduce + (map * x x))
        syy (reduce + (map * y y))]
    (/ (- (* n sxy) (* sx sy))
       (Math/sqrt (* (- (* n sxx) (square sx)) (- (* n syy) (square sy)))))))

(defn pearson-similarity
  "pearson correlation coefficient between two maps of keys->floats"
  [a b]
  { :pre [(map? a) (map? b)] }
  (let [common-keys (intersection (set (keys a)) (set (keys b)))]
    (pearson-correlation (vals-for-keys a common-keys) (vals-for-keys b common-keys))))

;; TODO: other metrics: jaccard coefficient, Manhattan distance

(defn top-matches [person-ratings other-ratings top-n similarity]
  (take top-n
    (sort-by :score >
      (map #(hash-map :name %, :score (similarity person-ratings (other-ratings %)))
           (keys other-ratings)))))

;; TODO: these would be less awkward with a relational data model
(defn similarities-to-others [person-ratings other-ratings similarity]
  (into {}
    (map #(vector % (similarity person-ratings (other-ratings %)))
         (keys other-ratings))))

(defn score-for-thing [thing other-ratings similarities]
  (let [scaled-ratings  (map (fn [[k v]] [(* (v thing) (similarities k)) (similarities k)])
                             (filter (fn [[k v]] (contains? v thing)) other-ratings))]
  (/ (reduce + (map first scaled-ratings)) (reduce + (map second scaled-ratings)))))

(defn recommend [person-ratings other-ratings similarity]
  (let [similarities (similarities-to-others person-ratings other-ratings similarity)
        things (difference (set (mapcat keys (vals other-ratings))) (set (keys person-ratings)))]
    (into {}
      (map
        #(vector % (score-for-thing % other-ratings similarities)) things))))

(defn transform-prefs [ratings]
  (let [things (set (mapcat keys (vals ratings)))]
    (into {}
      (map
        #(vector % (reduce (fn [m [k v]] (if (v %) (assoc m k (v %)) m)) {} ratings))
        things))))

;; At page 22 in Prog Coll Intelligence, can implement item-based filtering now
