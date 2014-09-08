(ns algorithms.stg.interpreter)

(defrecord STG [code argument-stack return-stack update-stack heap globals])
(defrecord Closure [lambda free-values])
(defrecord Lambda [free-variables update? closed-variables expression])

;; code:
;;  Eval e p (evaluate e in env p with args on stack)
;;  Enter a (apply closure at address a to args on stack)
;;  ReturnCon c ws (apply constructor c to values ws to the continuation on return stack)
;;  ReturnInt k (return integer k to the continuation on return stack)

;; local env p maps variable names to values
;; values: [:addr a], [:int n]
;; argument stack is a seq of values
;; heap is addr->closure
;; globals is variable-name->address of closure in heap

(defn value [local-env global-env v]
  (cond
    (integer? v) v
    (contains? local-env v) (local-env v)
    :else (global-env v)))

(defn values [local-env global-env vs]
  (map (partial value local-env global-env) vs))

(defn evaluate [stg]
  (let [instruction (:code stg)]
    (cond-match instruction
      [:eval [:apply ?f ?args] ?locals]
        (if-match [[:addr ?a] (value locals (:globals stg) f)]
          (STG. [:enter a]
                (concat (values locals (:globals stg) args) (:argument-stack stg))
                return-stack update-stack heap globals))
      [:enter ?a]
        (let [closure ((:heap stg) a)
              lambda  (:lambda closure)]
          (if (>= (count (:argument-stack stg))
                  (count (:closed-variables lambda)))
              (let [locals  (merge
                              (zipmap (:free-variables lambda) (:free-values closure))
                              (zipmap (:closed-variables lambda) (take (count (:closed-variables lambda)) (:argument-stack stg))))]
                (STG. [:eval (:expression closure) locals]
                      (drop (count (:closed-variables lambda)) (:argument-stack stg))
                      (:return-stack stg)
                      (:update-stack stg)
                      (:heap stg)
                      (:globals stg))))))))

(defn- third [coll] (nth coll 2))

(defn init-stg [definitions]
  (let [addresses (map #(vector %1 (name (gensym)) %2) definitions)
        heap      (into {} (map #(vector (first %) [:addr (second %)]) addresses))
        globals   (into {} (map #(vector (second %) (third %)) addresses))]
  (STG. [:eval [:apply "main" []] []]
        [] [] []
        heap globals)))
