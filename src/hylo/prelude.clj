(ns hylo.prelude
  (:require [hylo.adt :refer :all]
            [hylo.core :refer :all]))

(hylo_require)

(hylo
 8
 (let [id (fn [x] x)]
   (id 8)))

(identity hylo-environment)


(defadt maybe
  nothing
  (just value))

#_(def root-env
    {`sqrt
     (scheme [] (t-fun (t-prim :long) (t-prim :long)))
     })
