(ns hylo.core-test
  (:require [clojure.test :refer :all]
            [hylo.core :refer :all]
            [hylo.types :refer :all]))

(deftest a-test
  (testing "doesn't crash"
    (doseq [exp [(e-lit :long 8)
                 (e-fn 'x (e-sym 'x))
                 (e-let 'id (e-fn 'x (e-sym 'x)) (e-sym 'id))]]
      (-> (type-inference {} exp)
          show-type))
    (is true)))
