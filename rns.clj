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
(def qs [2 3 5 7])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;rns/qs</span>","value":"#'rns/qs"}
;; <=

;; @@
(def n (* 67 107))
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
(map #(vector (valid? % 31 n) %) (range 1 31))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"[() 1]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[() 2]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"}],"value":"[() 3]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"}],"value":"[() 4]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>67</span>","value":"67"}],"value":"(67)"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"}],"value":"[(67) 5]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"}],"value":"[() 6]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>7</span>","value":"7"}],"value":"[() 7]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"}],"value":"[() 8]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>9</span>","value":"9"}],"value":"[() 9]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"}],"value":"[() 10]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>11</span>","value":"11"}],"value":"[() 11]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>12</span>","value":"12"}],"value":"[() 12]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>13</span>","value":"13"}],"value":"[() 13]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>107</span>","value":"107"}],"value":"(107)"},{"type":"html","content":"<span class='clj-long'>14</span>","value":"14"}],"value":"[(107) 14]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>15</span>","value":"15"}],"value":"[() 15]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>16</span>","value":"16"}],"value":"[() 16]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>17</span>","value":"17"}],"value":"[() 17]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>18</span>","value":"18"}],"value":"[() 18]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>19</span>","value":"19"}],"value":"[() 19]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>20</span>","value":"20"}],"value":"[() 20]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>21</span>","value":"21"}],"value":"[() 21]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>22</span>","value":"22"}],"value":"[() 22]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>23</span>","value":"23"}],"value":"[() 23]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>24</span>","value":"24"}],"value":"[() 24]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>25</span>","value":"25"}],"value":"[() 25]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>26</span>","value":"26"}],"value":"[() 26]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>27</span>","value":"27"}],"value":"[() 27]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>28</span>","value":"28"}],"value":"[() 28]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>29</span>","value":"29"}],"value":"[() 29]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[],"value":"()"},{"type":"html","content":"<span class='clj-long'>30</span>","value":"30"}],"value":"[() 30]"}],"value":"([() 1] [() 2] [() 3] [() 4] [(67) 5] [() 6] [() 7] [() 8] [() 9] [() 10] [() 11] [() 12] [() 13] [(107) 14] [() 15] [() 16] [() 17] [() 18] [() 19] [() 20] [() 21] [() 22] [() 23] [() 24] [() 25] [() 26] [() 27] [() 28] [() 29] [() 30])"}
;; <=

;; @@
(crt (residues 67 qs) qs)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>67</span>","value":"67"}
;; <=

;; @@
(crt (complements n (residues 67 (conj qs 13)) (conj qs 13)) (conj qs 13))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>107</span>","value":"107"}
;; <=

;; @@
(residues 67 (conj qs 13))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"[1 1 2 4 2]"}
;; <=

;; @@

;; @@
