(ns hylo.core-test
  (:require [clojure.test :refer :all]
            [hylo.core :refer :all]
            [hylo.types :refer :all]))

(deftest a-test
  (testing "doesn't crash"
    (map (comp prn show-type (partial type-inference empty-env))
         [(e-lit (l-int 8))
          (e-abs "x" (e-var "x"))])
    (is true)))
