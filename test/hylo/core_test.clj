(ns hylo.core-test
  (:require [clojure.test :refer :all]
            [hylo.core :refer :all]
            [hylo.types :refer :all]))

(deftest ir-test
  (testing "doesn't crash"
    (doseq [exp [(e-lit :long 8)
                 (e-fn 'x (e-sym 'x))
                 (e-let 'id (e-fn 'x (e-sym 'x)) (e-sym 'id))]]
      (-> (type-inference {} exp)
          show-type
          println))
    (is true)))

(def exprs '[8
             (fn [x] x)
             (let [id (fn [x] x)] id)
             (let [id (fn [x] x)]
               (id id))
             (let [id (fn [x] (let [y x] y))]
               (id id))
             (let [id (fn [x] (let [y x] y))]
               ((id id) 2))
             #_(let [id (fn [x] (x x))]
                 id)
             (fn [m] (let [y m]
                       (let [x (y true)]
                         x)))
             []
             (let [id (fn [x] x)] [])
             [:a 8]
             (fn [x] [x x])
             (fn [y] ((fn [x] [x x]) y))
             (fn [m] (let [y m]
                       (let [x (y [])]
                         x)))
             ])

(deftest clj-test
  (testing "doesn't crash"
    (binding [*print-meta* true]
      (doseq [exp exprs]
        (->> (clj->ir exp)
             (type-inference {})
             show-type
             println)))
    (is true)))

(deftest exp-test
  (testing "doesn't crash"
    (binding [*print-meta* true]
      (doseq [exp exprs]
        (->> (clj->ir exp)
             (annotate-expression {})
             show-exp
             println)))
    (is true)))
