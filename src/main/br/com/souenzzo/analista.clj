(ns br.com.souenzzo.analista
  (:require [clara.rules :as clara]
            [clojure.string :as string])
  (:import (java.util UUID)))

(defrecord Eav [e a v])

(clara/defrule OpDo
  [Eav (= e ?e) (= a :form) (= ?v v)]
  [:and
   [:test (list? ?v)]
   [:test (= "do" (name (first ?v)))]]
  =>
  (clara/insert! (->Eav ?e :op :do)))


(clara/defrule OpInvoke
  {:salience 10}
  [Eav (= e ?e) (= a :form) (= ?v v)]
  [:and
   [:test (list? ?v)]
   [:test (qualified-symbol? (first ?v))]]
  =>
  (doseq [i (rest ?v)
          :let [id (UUID/randomUUID)]]
    (clara/insert! (->Eav ?e :args id)
                   (->Eav id :form i)))
  (let [id (UUID/randomUUID)]
    (clara/insert! (->Eav ?e :children :fn)
                   (->Eav ?e :children :args)
                   (->Eav ?e :fn id)
                   (->Eav id :form (first ?v))
                   (->Eav ?e :op :invoke))))

(clara/defrule OpVar
  [Eav (= e ?e) (= a :form) (= ?v v)]
  [:and
   [:test (qualified-symbol? ?v)]]
  =>
  (let [id (UUID/randomUUID)
        ns (symbol (string/replace (namespace (symbol ?v))
                                   #"^clojure\."
                                   "cljs."))
        name (symbol (name ns)
                     (name ?v))]
    (clara/insert! (->Eav ?e :op :var)
                   (->Eav id :name name)
                   (->Eav id :ns ns)
                   (->Eav id :op :var)
                   (->Eav ?e :info id)
                   (->Eav ?e :name name)
                   (->Eav ?e :ns ns))))


(clara/defrule OpQuote
  [Eav (= e ?e) (= a :form) (= ?v v)]
  [:and
   [:test (seq? ?v)]
   [:test (= "quote" (name (first ?v)))]]
  =>
  (clara/insert! (->Eav ?e :op :quote)))



(clara/defrule OpVector
  [Eav (= e ?e) (= a :form) (= ?v v)]
  [:and
   [:test (vector? ?v)]]
  =>
  (clara/insert! (->Eav ?e :op :vector)))

(clara/defrule OpMap
  [Eav (= e ?e) (= a :form) (= ?v v)]
  [:and
   [:test (map? ?v)]]
  =>
  (clara/insert! (->Eav ?e :op :map)))

(clara/defrule OpConst
  [Eav (= e ?e) (= a :form) (= ?v v)]
  [:test (and (not (coll? ?v))
              (not (qualified-symbol? ?v)))]
  =>
  (clara/insert! (->Eav ?e :op :const)))


(clara/defrule StatementsDo
  [Eav (= e ?e) (= a :op) (= v :do)]
  [Eav (= e ?e) (= a :form) (= ?v v)]
  =>
  (clara/insert! (->Eav ?e :children :statements))
  (doseq [i (butlast (rest ?v))
          :let [id (UUID/randomUUID)]]
    (clara/insert! (->Eav ?e :statements id)
                   (->Eav id :form i))))

(clara/defrule ExprQuote
  [Eav (= e ?e) (= a :op) (= v :quote)]
  [Eav (= e ?e) (= a :form) (= ?v v)]
  =>
  (clara/insert! (->Eav ?e :children :expr))
  (let [i (second ?v)
        id (UUID/randomUUID)]
    (clara/insert! (->Eav ?e :expr id)
                   (->Eav id :form i)
                   (->Eav id :literal? true))))


(clara/defrule KVsMap
  [Eav (= e ?e) (= a :op) (= v :map)]
  [Eav (= e ?e) (= a :form) (= ?v v)]
  =>
  (clara/insert! (->Eav ?e :children :keys)
                 (->Eav ?e :children :vals))
  (doseq [[k v] ?v
          :let [kid (UUID/randomUUID)
                vid (UUID/randomUUID)]]
    (clara/insert! (->Eav ?e :keys kid)
                   (->Eav kid :form k)
                   (->Eav ?e :vals vid)
                   (->Eav vid :form v))))

(clara/defrule VetorItems
  [Eav (= e ?e) (= a :op) (= v :vector)]
  [Eav (= e ?e) (= a :form) (= ?v v)]
  =>
  (clara/insert! (->Eav ?e :children :items))
  (doseq [i ?v
          :let [id (UUID/randomUUID)]]
    (clara/insert! (->Eav ?e :items id)
                   (->Eav id :form i))))



(clara/defrule RetDo
  [Eav (= e ?e) (= a :op) (= v :do)]
  [Eav (= e ?e) (= a :form) (= ?v v)]
  =>
  (let [i (last (rest ?v))
        id (UUID/randomUUID)]
    (clara/insert! (->Eav ?e :ret id)
                   (->Eav ?e :children :ret)
                   (->Eav id :form i))))


(clara/defrule ValConst
  [Eav (= e ?e) (= a :op) (= v :const)]
  [Eav (= e ?e) (= a :form) (= ?v v)]
  =>
  (clara/insert! (->Eav ?e :val ?v)))

(clara/defrule TagValNumber
  [Eav (= e ?e) (= a :val) (= v ?v)]
  [:test (number? ?v)]
  =>
  (clara/insert! (->Eav ?e :tag 'number)))

(clara/defrule TagInvokeDefault
  [Eav (= e ?e) (= a :op) (= v :invoke)]
  =>
  (clara/insert! (->Eav ?e :tag 'any)))

(clara/defrule TagValNil
  [Eav (= e ?e) (= a :val) (= v ?v)]
  [:test (nil? ?v)]
  =>
  (clara/insert! (->Eav ?e :tag 'clj-nil)))

(clara/defrule TagValString
  [Eav (= e ?e) (= a :val) (= v ?v)]
  [:test (string? ?v)]
  =>
  (clara/insert! (->Eav ?e :tag 'string)))

(clara/defrule TagValKeyword
  [Eav (= e ?e) (= a :val) (= v ?v)]
  [:test (keyword? ?v)]
  =>
  (clara/insert! (->Eav ?e :tag 'cljs.core/Keyword)))

(clara/defrule TagValSymbol
  [Eav (= e ?e) (= a :val) (= v ?v)]
  [:test (symbol? ?v)]
  =>
  (clara/insert! (->Eav ?e :tag 'cljs.core/Symbol)))

(clara/defrule TagVector
  [Eav (= e ?e) (= a :op) (= v :vector)]
  =>
  (clara/insert! (->Eav ?e :tag 'cljs.core/IVector)))

(clara/defrule MapTag
  [Eav (= e ?e) (= a :op) (= v :map)]
  =>
  (clara/insert! (->Eav ?e :tag 'cljs.core/IMap)))

(clara/defrule DoTag
  [Eav (= e ?e) (= a :op) (= v :do)]
  [Eav (= e ?e) (= a :ret) (= v ?ret)]
  [Eav (= e ?ret) (= a :tag) (= v ?tag)]
  =>
  (clara/insert! (->Eav ?e :tag ?tag)))

(clara/defrule QuoteTag
  [Eav (= e ?e) (= a :op) (= v :quote)]
  [Eav (= e ?e) (= a :expr) (= v ?ret)]
  [Eav (= e ?ret) (= a :tag) (= v ?tag)]
  =>
  (clara/insert! (->Eav ?e :tag ?tag)))



(clara/defquery ById
  ""
  [:?id]
  [?eav <- Eav (= ?id e)])

(def many? #{:statements :items :keys :vals :children :args})

(defn session->ana
  [session id]
  (let [{:keys [children]
         :as   result} (reduce (fn [acc {:keys [e a v]}]
                                 (if (many? a)
                                   (update acc a (fnil conj [])
                                           (if (uuid? v)
                                             (session->ana session v)
                                             v))
                                   (assoc acc a (if (uuid? v)
                                                  (session->ana session v)
                                                  v))))
                               {}
                               (map :?eav (clara/query session ById :?id id)))]
    (if (empty? children)
      result
      (reduce (fn [acc k]
                (update acc k (fnil identity [])))
              result
              children))))

(defn analyze
  [{::keys [session]} form]
  (let [id (UUID/randomUUID)]
    (-> session
        (clara/insert (->Eav id :form form))
        (clara/fire-rules)
        (session->ana id))))