(ns br.com.souenzzo.analista-test
  (:require [clojure.test :refer [deftest is are testing]]
            [br.com.souenzzo.analista :as souenzzo.ana]
            [cljs.analyzer.api :as cljs.ana]
            [clojure.walk :as walk]
            [clara.rules :as clara])
  (:import (br.com.souenzzo.analista Eav)))

(clara/defrule InferGet
  [Eav (= e ?e) (= a :op) (= v :invoke)]
  [Eav (= e ?e) (= a :fn) (= v ?fn)]
  [Eav (= e ?fn) (= a :name) (= v 'cljs.core/get)]
  [Eav (= e ?e) (= a :form) (= v ?v)]
  [:and
   [:test (map? (second ?v))]
   [:test (= 3 (count ?v))]
   [:test (empty? (second ?v))]]
  =>
  (clara/insert! (souenzzo.ana/->Eav ?e :tag 'clj-nil)))

(defn without
  [m & ks]
  (walk/postwalk (fn [x]
                   (if (map? x)
                     (apply dissoc x ks)
                     x))
                 m))

(defmacro cljs
  [form]
  `'~(-> (cljs.ana/analyze &env form)
         (cljs.ana/no-warn)
         (without :env :cljs.analyzer/analyzed)))

(defmacro ana
  [form]
  `'~(souenzzo.ana/analyze {::souenzzo.ana/session (clara/mk-session 'br.com.souenzzo.analista)} form))

(deftest simple
  (are [a] (= (cljs a)
              (ana a))
           1
           (do 1)
           (do)
           {1 2}
           [1 2]
           "a"
           :foo
           'foo
           (clojure.core/get {} :a)))

(deftest extended
  (testing
    "default behavior"
    (is (= (:tag (ana (clojure.core/get {} :a)))
           'any)))
  (testing
    "behavior with the new rule"
    (is (= (:tag (souenzzo.ana/analyze {::souenzzo.ana/session (clara/mk-session 'br.com.souenzzo.analista
                                                                                 'br.com.souenzzo.analista-test)}
                                      '(clojure.core/get {} :a)))
           'clj-nil))
    (is (= (:tag (souenzzo.ana/analyze {::souenzzo.ana/session (clara/mk-session 'br.com.souenzzo.analista
                                                                                 'br.com.souenzzo.analista-test)}
                                       '(clojure.core/get {:foo 33} :a)))
           'any))))
