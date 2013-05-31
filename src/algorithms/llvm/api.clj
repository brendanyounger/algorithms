(ns algorithms.llvm.api
  (import [com.sun.jna Function Pointer]))

(System/setProperty "jna.platform.library.path" "/usr/local/lib:/usr/lib")

(defn- import-llvm-c-func [lib name ret-type num-args]
  (let [fn-name (symbol name)]
    (eval
     `(let [function# (Function/getFunction ~lib ~name)]
        (defn ~fn-name [& args#]
          (assert (= ~num-args (count args#)))
          (.invoke function# ~ret-type (to-array args#)))))))

(let [llvm-api [["LLVMDoubleType" Pointer 0]
                ["LLVMInt32Type" Pointer 0]
                ["LLVMInt64Type" Pointer 0]
                ["LLVMConstString" Pointer 3]
                ["LLVMRecompileAndRelinkFunction" Pointer 2]
                ["LLVMConstInt" Pointer 3]
                ["LLVMConstReal" Pointer 2]
                ["LLVMModuleCreateWithName" Pointer 1]
                ["LLVMBuildCall" Pointer 5]
                ["LLVMDisposeModule" Boolean 1]
                ["LLVMBuildRet" Pointer 2]
                ["LLVMDumpModule" Void 1]
                ["LLVMWriteBitcodeToFile" Integer 2]
                ["LLVMCreateBuilder" Pointer 0]
                ["LLVMBuildGlobalString" Pointer 3]
                ["LLVMBuildGlobalStringPtr" Pointer 3]
                ["LLVMAppendBasicBlock" Pointer 2]
                ["LLVMPositionBuilderAtEnd" Pointer 2]
                ["LLVMTypeOf" Pointer 1]
                ["LLVMFunctionType" Pointer 4]
                ["LLVMAddFunction" Pointer 3]]]
  (doseq [[name ret-type num-args] llvm-api]
    (import-llvm-c-func "LLVM-3.2svn" name ret-type num-args)))

(declare wrap-type)

(defn- sanitize-array [arr]
  (when (seq arr) (into-array arr)))

(defn- wrap-func-type [[ret params]]
  (let [func-type (fn [params variadic?]
                      (LLVMFunctionType (wrap-type ret)
                                        (sanitize-array (map wrap-type params))
                                        (count params)
                                        variadic?))]
    (if (= (last params) :...)
        (func-type (drop-last params) true)
        (func-type params false))))

(defn wrap-type [clj-type]
  (let
    [type-map
      { Double  (LLVMDoubleType)
        Integer (LLVMInt32Type)
        Long    (LLVMInt64Type)
        String  (LLVMTypeOf (LLVMConstString "hello" 5 false)) }
    lookup (type-map clj-type)]
    (cond
      (sequential? clj-type) (wrap-func-type clj-type)
      lookup lookup
      :else (assert false (str clj-type)))))

(defmulti wrap-value class)
(defmethod wrap-value Double [v]
           (LLVMConstReal (wrap-type (class v)) v))
(defmethod wrap-value String [v]
           (LLVMConstString v (count v) false))
(defmethod wrap-value Integer [v]
           (LLVMConstInt (wrap-type (class v)) v false))
(defmethod wrap-value Long [v]
           (LLVMConstInt (wrap-type (class v)) v false))
(defmethod wrap-value :default [v] v)

(defn build-call [builder fn args]
  (LLVMBuildCall builder fn (sanitize-array (map wrap-value args)) (count args) ""))

;; type = [return-type [param-type1 param-type2]]
(defn add-function [mod name type]
  (LLVMAddFunction mod name (wrap-type type)))
