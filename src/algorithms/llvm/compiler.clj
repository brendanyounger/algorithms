(ns algorithms.llvm.compiler
  (require [algorithms.llvm.api :as api]))

(def ast
  '[:call printf "Hello, world!"])

;; TODO: need to wrap all of this into a function and do a clojure.walk prewalk on it
(defn- compile-statement [builder stmt]
  (cond
    (and (seq? stmt) (= (first stmt) :call))
      (build-call builder (wrap-value (first stmt)) (map wrap-value (rest stmt)))
    :else (wrap-value stmt)))

(defn compile [ast]
  (let [module  (LLVMModuleCreateWithName "main")
        builder (LLVMCreateBuilder)]
    (add-function module "printf" [java.lang.Integer [java.lang.String :...]])

    ))
