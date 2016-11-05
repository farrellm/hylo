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

(deftest clj-test
  (testing "doesn't crash"
    (doseq [exp '[8
                  (fn [x] x)
                  (let [id (fn [x] x)] id)]]
      (->> (clj->ir exp)
           (type-inference {})
           show-type
           println))
    (is true)))
