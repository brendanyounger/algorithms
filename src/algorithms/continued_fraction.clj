(ns algorithms.continued-fraction)

(defn square-free-impl [N]
  (loop [i (long (Math/sqrt (Math/abs N)))
         n (Math/abs N)]
    (cond
      (< i 2)
        [(/ (Math/abs N) n) (if (< N 0) (- n) n)]
      (= 0 (rem n (* i i)))
        (recur (min (dec i) (quot n (* i i)))  (quot n (* i i)))
      :else
        (recur (dec i) n))))

(def square-free (memoize square-free-impl))

(defn fundamental-discriminant? [D]
  (and
    (>= 4 (first (square-free D)))
    (or
      (= 1 (mod D 4))
      (= 8 (mod D 16))
      (= 12 (mod D 16)))))

;; calculate N(a+bw) where w = f*w_0
(defn norm [a b D]
  (let [[f2 D0] (square-free D)]
    (+ (* a a) (* b b f2 (if (= 1 (mod D0 4)) (/ (- 1 D0) 4) (- D0))))))

;; ideals in order O look like [a, b + cw] s.t. a > 0, c > 0, 0 <= b < a, c | a, c | b, and ac | N(b+cw)
(defn ideal? [a b c D]
  (and
    (> a 0)
    (> c 0)
    (<= 0 b)
    (< b a)
    (= 0 (rem a c))
    (= 0 (rem b c))
    (= 0 (rem (norm b c D) (* a c)))))

(defn cont-frac-ideal [a b c D]
  (let [[f2 D0] (square-free D)
        f (int (Math/sqrt f2))
        r (if (= 1 (mod D0 4)) 2 1)
        S c
        Q (/ (* r a) c)
        P (+ (/ (* r b) c) (* f (- r 1)))]
    (list '* S [(list '/ Q r) (list '/ (list '+ P (list 'sqrt D)) r)])))

(defn params [N]
  (let [[f2 D0] (square-free N)
        f (int (Math/sqrt f2))
        r (if (= 1 (mod D0 4)) 2 1)
        fundamental-discriminant (* (/ 2 r) (/ 2 r) D0)
        w_0 (if (= 1 r) (list 'sqrt D0) (list '/ (list 1 '+ (list 'sqrt D0)) 2))
        w_0_conj (if (= 1 r) (list '- (list 'sqrt D0)) (list '/ (list 1 '- (list 'sqrt D0)) 2))]
    { :D (if (= 1 (mod N 4)) N (* 4 N)) ;; b^2 - 4ac
      :D_0 D0
      :f f
      :r r

      :𝜔_0 w_0
      :𝜔_0_conj w_0_conj
      :𝚫_0 fundamental-discriminant ;; fundamental discriminant of Q(sqrt(D0)), also (w0 - conj(w0))^2

      :𝜔 (list '* f w_0)
      :𝜔_conj (list '* f w_0_conj)
      ;; orders O of Q(sqrt(D0)) correspond to [1, f*w_0] = [1, w] where 𝚫(O) = f^2*𝚫 (the fundamental discriminant) and f is the conductor of the order
      :order [1, (list '* f w_0)]
    }))

