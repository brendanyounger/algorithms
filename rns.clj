;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit ctrl+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns rns
  (:require [gorilla-plot.core :as plot]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(def p 21713)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rns/p</span>","value":"#'rns/p"}
;; <=

;; @@
(def q 71359)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rns/q</span>","value":"#'rns/q"}
;; <=

;; @@
(def N (* p q))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rns/N</span>","value":"#'rns/N"}
;; <=

;; @@
N
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>1549417967</span>","value":"1549417967"}
;; <=

;; @@
(< N (apply * [2 3 5 7 11 13 17 19 23 29]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}
;; <=

;; @@
(def ps [2 3 5 7 11 13 17 19 23 29])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rns/ps</span>","value":"#'rns/ps"}
;; <=

;; @@
(defn egcd [a b]
  (if (= 0 a)
    [b 0 1]
    (let [[g y x] (egcd (mod b a) a)]
      [g (- x (* (quot b a) y)) y])))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rns/egcd</span>","value":"#'rns/egcd"}
;; <=

;; @@
(defn inverse [x m] (let [[g a b] (egcd x m)] (if (= 1 g) (mod a m))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rns/inverse</span>","value":"#'rns/inverse"}
;; <=

;; @@
(defn crt [xs ps] (let [M (apply * ps)] (mod (reduce + (map * xs (map #(* (quot M %) (inverse (quot M %) %)) ps))) M)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rns/crt</span>","value":"#'rns/crt"}
;; <=

;; @@
(defn residues [n ps] (mapv (partial mod n) ps))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rns/residues</span>","value":"#'rns/residues"}
;; <=

;; @@
(defn complements [n xs ps]
  (mapv #(mod (* n (inverse %1 %2)) %2) xs ps))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rns/complements</span>","value":"#'rns/complements"}
;; <=

;; @@
(complements (* 67 107) [1 1 3 4] [2 3 5 7])
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[1 2 3 2]"}
;; <=

;; @@
(defn cross [& seqs]
 (when seqs
  (if-let [s (first seqs)]
    (if-let [ss (next seqs)]
      (for [x  s
            ys (apply cross ss)]
        (cons x ys))
      (map list s)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rns/cross</span>","value":"#'rns/cross"}
;; <=

;; @@
(take 10 (apply cross (map range ps)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"(0 0 0 0 0 0 0 0 0 0)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"(0 0 0 0 0 0 0 0 0 1)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"(0 0 0 0 0 0 0 0 0 2)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"(0 0 0 0 0 0 0 0 0 3)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"}],"value":"(0 0 0 0 0 0 0 0 0 4)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"}],"value":"(0 0 0 0 0 0 0 0 0 5)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"}],"value":"(0 0 0 0 0 0 0 0 0 6)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>7</span>","value":"7"}],"value":"(0 0 0 0 0 0 0 0 0 7)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"}],"value":"(0 0 0 0 0 0 0 0 0 8)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>9</span>","value":"9"}],"value":"(0 0 0 0 0 0 0 0 0 9)"}],"value":"((0 0 0 0 0 0 0 0 0 0) (0 0 0 0 0 0 0 0 0 1) (0 0 0 0 0 0 0 0 0 2) (0 0 0 0 0 0 0 0 0 3) (0 0 0 0 0 0 0 0 0 4) (0 0 0 0 0 0 0 0 0 5) (0 0 0 0 0 0 0 0 0 6) (0 0 0 0 0 0 0 0 0 7) (0 0 0 0 0 0 0 0 0 8) (0 0 0 0 0 0 0 0 0 9))"}
;; <=

;; @@
(def qs [2 3 5 7 11])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rns/qs</span>","value":"#'rns/qs"}
;; <=

;; @@
(* 2 3 5 7 11)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>2310</span>","value":"2310"}
;; <=

;; @@
(def n (* 1699 2089 ))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rns/n</span>","value":"#'rns/n"}
;; <=

;; @@
(defn valid? [a m n]
  (sort (map #(crt % qs) 
           (filter #(and (= (crt % qs) (crt (conj (vec %) a) (conj qs m)))
                         (= (crt (complements n % qs) qs) 
                            (crt (complements n (conj (vec %) a) (conj qs m)) (conj qs m))))
                   (apply cross (map (partial range 1) qs))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rns/valid?</span>","value":"#'rns/valid?"}
;; <=

;; @@
(map #(vector (valid? % 29 n) %) (range 1 29))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>523</span>","value":"523"},{"type":"html","content":"<span class='clj-long'>1451</span>","value":"1451"},{"type":"html","content":"<span class='clj-long'>2089</span>","value":"2089"}],"value":"(523 1451 2089)"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[(523 1451 2089) 1]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[() 2]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[() 3]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"}],"value":"[() 4]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>527</span>","value":"527"},{"type":"html","content":"<span class='clj-long'>1223</span>","value":"1223"}],"value":"(527 1223)"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"}],"value":"[(527 1223) 5]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1543</span>","value":"1543"}],"value":"(1543)"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"}],"value":"[(1543) 6]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>703</span>","value":"703"}],"value":"(703)"},{"type":"html","content":"<span class='clj-long'>7</span>","value":"7"}],"value":"[(703) 7]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"}],"value":"[() 8]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>9</span>","value":"9"}],"value":"[() 9]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"}],"value":"[() 10]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1171</span>","value":"1171"}],"value":"(1171)"},{"type":"html","content":"<span class='clj-long'>11</span>","value":"11"}],"value":"[(1171) 11]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1781</span>","value":"1781"}],"value":"(1781)"},{"type":"html","content":"<span class='clj-long'>12</span>","value":"12"}],"value":"[(1781) 12]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>13</span>","value":"13"}],"value":"[() 13]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>797</span>","value":"797"},{"type":"html","content":"<span class='clj-long'>2131</span>","value":"2131"}],"value":"(797 2131)"},{"type":"html","content":"<span class='clj-long'>14</span>","value":"14"}],"value":"[(797 2131) 14]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1523</span>","value":"1523"},{"type":"html","content":"<span class='clj-long'>1697</span>","value":"1697"}],"value":"(1523 1697)"},{"type":"html","content":"<span class='clj-long'>15</span>","value":"15"}],"value":"[(1523 1697) 15]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>16</span>","value":"16"}],"value":"[() 16]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1061</span>","value":"1061"},{"type":"html","content":"<span class='clj-long'>1699</span>","value":"1699"},{"type":"html","content":"<span class='clj-long'>2047</span>","value":"2047"}],"value":"(1061 1699 2047)"},{"type":"html","content":"<span class='clj-long'>17</span>","value":"17"}],"value":"[(1061 1699 2047) 17]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>18</span>","value":"18"}],"value":"[() 18]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1237</span>","value":"1237"}],"value":"(1237)"},{"type":"html","content":"<span class='clj-long'>19</span>","value":"19"}],"value":"[(1237) 19]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>571</span>","value":"571"}],"value":"(571)"},{"type":"html","content":"<span class='clj-long'>20</span>","value":"20"}],"value":"[(571) 20]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>21</span>","value":"21"}],"value":"[() 21]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>22</span>","value":"22"}],"value":"[() 22]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>23</span>","value":"23"}],"value":"[() 23]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>691</span>","value":"691"},{"type":"html","content":"<span class='clj-long'>923</span>","value":"923"}],"value":"(691 923)"},{"type":"html","content":"<span class='clj-long'>24</span>","value":"24"}],"value":"[(691 923) 24]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>25</span>","value":"25"}],"value":"[() 25]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>26</span>","value":"26"}],"value":"[() 26]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>607</span>","value":"607"}],"value":"(607)"},{"type":"html","content":"<span class='clj-long'>27</span>","value":"27"}],"value":"[(607) 27]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1391</span>","value":"1391"}],"value":"(1391)"},{"type":"html","content":"<span class='clj-long'>28</span>","value":"28"}],"value":"[(1391) 28]"}],"value":"([(523 1451 2089) 1] [() 2] [() 3] [() 4] [(527 1223) 5] [(1543) 6] [(703) 7] [() 8] [() 9] [() 10] [(1171) 11] [(1781) 12] [() 13] [(797 2131) 14] [(1523 1697) 15] [() 16] [(1061 1699 2047) 17] [() 18] [(1237) 19] [(571) 20] [() 21] [() 22] [() 23] [(691 923) 24] [() 25] [() 26] [(607) 27] [(1391) 28])"}
;; <=

;; @@
(mod (* 919 529) 17)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}
;; <=

;; @@
(defn valid1 [a m n]
  (sort (map #(crt % qs) 
           (filter #(and (= (crt % qs) (crt (conj (vec %) a) (conj qs m)))
                         ;(= (mod (crt (complements n % qs) qs) m) (first (complements n [a] [m])))
                         )
                   (apply cross (map (partial range 1) qs))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rns/valid1</span>","value":"#'rns/valid1"}
;; <=

;; @@
(map #(vector (valid1 % 11 n) %) (range 1 11))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>23</span>","value":"23"},{"type":"html","content":"<span class='clj-long'>67</span>","value":"67"},{"type":"html","content":"<span class='clj-long'>89</span>","value":"89"},{"type":"html","content":"<span class='clj-long'>199</span>","value":"199"}],"value":"(1 23 67 89 199)"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[(1 23 67 89 199) 1]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>13</span>","value":"13"},{"type":"html","content":"<span class='clj-long'>79</span>","value":"79"},{"type":"html","content":"<span class='clj-long'>101</span>","value":"101"},{"type":"html","content":"<span class='clj-long'>167</span>","value":"167"}],"value":"(13 79 101 167)"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[(13 79 101 167) 2]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>47</span>","value":"47"},{"type":"html","content":"<span class='clj-long'>113</span>","value":"113"},{"type":"html","content":"<span class='clj-long'>157</span>","value":"157"},{"type":"html","content":"<span class='clj-long'>179</span>","value":"179"}],"value":"(47 113 157 179)"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[(47 113 157 179) 3]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>37</span>","value":"37"},{"type":"html","content":"<span class='clj-long'>59</span>","value":"59"},{"type":"html","content":"<span class='clj-long'>103</span>","value":"103"},{"type":"html","content":"<span class='clj-long'>169</span>","value":"169"},{"type":"html","content":"<span class='clj-long'>191</span>","value":"191"}],"value":"(37 59 103 169 191)"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"}],"value":"[(37 59 103 169 191) 4]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>71</span>","value":"71"},{"type":"html","content":"<span class='clj-long'>137</span>","value":"137"},{"type":"html","content":"<span class='clj-long'>181</span>","value":"181"}],"value":"(71 137 181)"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"}],"value":"[(71 137 181) 5]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>17</span>","value":"17"},{"type":"html","content":"<span class='clj-long'>61</span>","value":"61"},{"type":"html","content":"<span class='clj-long'>83</span>","value":"83"},{"type":"html","content":"<span class='clj-long'>127</span>","value":"127"},{"type":"html","content":"<span class='clj-long'>149</span>","value":"149"},{"type":"html","content":"<span class='clj-long'>193</span>","value":"193"}],"value":"(17 61 83 127 149 193)"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"}],"value":"[(17 61 83 127 149 193) 6]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>29</span>","value":"29"},{"type":"html","content":"<span class='clj-long'>73</span>","value":"73"},{"type":"html","content":"<span class='clj-long'>139</span>","value":"139"}],"value":"(29 73 139)"},{"type":"html","content":"<span class='clj-long'>7</span>","value":"7"}],"value":"[(29 73 139) 7]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>19</span>","value":"19"},{"type":"html","content":"<span class='clj-long'>41</span>","value":"41"},{"type":"html","content":"<span class='clj-long'>107</span>","value":"107"},{"type":"html","content":"<span class='clj-long'>151</span>","value":"151"},{"type":"html","content":"<span class='clj-long'>173</span>","value":"173"}],"value":"(19 41 107 151 173)"},{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"}],"value":"[(19 41 107 151 173) 8]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>31</span>","value":"31"},{"type":"html","content":"<span class='clj-long'>53</span>","value":"53"},{"type":"html","content":"<span class='clj-long'>97</span>","value":"97"},{"type":"html","content":"<span class='clj-long'>163</span>","value":"163"}],"value":"(31 53 97 163)"},{"type":"html","content":"<span class='clj-long'>9</span>","value":"9"}],"value":"[(31 53 97 163) 9]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>43</span>","value":"43"},{"type":"html","content":"<span class='clj-long'>109</span>","value":"109"},{"type":"html","content":"<span class='clj-long'>131</span>","value":"131"},{"type":"html","content":"<span class='clj-long'>197</span>","value":"197"}],"value":"(43 109 131 197)"},{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"}],"value":"[(43 109 131 197) 10]"}],"value":"([(1 23 67 89 199) 1] [(13 79 101 167) 2] [(47 113 157 179) 3] [(37 59 103 169 191) 4] [(71 137 181) 5] [(17 61 83 127 149 193) 6] [(29 73 139) 7] [(19 41 107 151 173) 8] [(31 53 97 163) 9] [(43 109 131 197) 10])"}
;; <=

;; @@
(map #(residues % qs) (range 1 200 11))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[1 1 1 1]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"}],"value":"[0 0 2 5]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[1 2 3 2]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"}],"value":"[0 1 4 6]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[1 0 0 3]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[0 2 1 0]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"}],"value":"[1 1 2 4]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[0 0 3 1]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"}],"value":"[1 2 4 5]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[0 1 0 2]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"}],"value":"[1 0 1 6]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[0 2 2 3]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[1 1 3 0]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"}],"value":"[0 0 4 4]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[1 2 0 1]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"}],"value":"[0 1 1 5]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[1 0 2 2]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"}],"value":"[0 2 3 6]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[1 1 4 3]"}],"value":"([1 1 1 1] [0 0 2 5] [1 2 3 2] [0 1 4 6] [1 0 0 3] [0 2 1 0] [1 1 2 4] [0 0 3 1] [1 2 4 5] [0 1 0 2] [1 0 1 6] [0 2 2 3] [1 1 3 0] [0 0 4 4] [1 2 0 1] [0 1 1 5] [1 0 2 2] [0 2 3 6] [1 1 4 3])"}
;; <=

;; @@
(defn full-crt [xs ps] (let [M (apply * ps)
                             A (reduce + (map * xs (map #(* (quot M %) (inverse (quot M %) %)) ps)))]
                         [(mod A M) (quot A M)]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rns/full-crt</span>","value":"#'rns/full-crt"}
;; <=

;; @@
(full-crt [1 2 3 4] qs)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>53</span>","value":"53"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"}],"value":"[53 5]"}
;; <=

;; @@
(def r11 (map #(mod (crt % qs) 11) (apply cross (map (partial range 1) qs))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rns/r11</span>","value":"#'rns/r11"}
;; <=

;; @@
(complements n [1 2 3 4] qs)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[1 1 3 2]"}
;; <=

;; @@
(mod (crt [1 1 3 2] qs) 11)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>9</span>","value":"9"}
;; <=

;; @@
(map #(residues % qs) (range 1 100 11))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[1 1 1 1]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"}],"value":"[0 0 2 5]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[1 2 3 2]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"}],"value":"[0 1 4 6]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[1 0 0 3]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"[0 2 1 0]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"}],"value":"[1 1 2 4]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[0 0 3 1]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"}],"value":"[1 2 4 5]"}],"value":"([1 1 1 1] [0 0 2 5] [1 2 3 2] [0 1 4 6] [1 0 0 3] [0 2 1 0] [1 1 2 4] [0 0 3 1] [1 2 4 5])"}
;; <=

;; @@

;; @@
