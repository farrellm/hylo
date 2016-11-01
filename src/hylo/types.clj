(ns hylo.types
  (:require [hylo.adt :refer :all]
            [clojure.set :as set]))

(defadt exp
  (e-var name)
  (e-lit lit)
  (e-app func arg)
  (e-abs s e)
  (e-let name val body))

(defadt lit
  (l-int i)
  (l-bool b))

(defadt type
  (t-var name)
  t-int
  t-bool
  (t-fun param ret))

(defadt scheme
  (scheme ns ty))


;;; instance Types Type
(defn ftv-type [t]
  (match t
    [::t-var n] #{n}
    [::t-int] #{}
    [::t-bool] #{}
    [::t-fun param ret] (set/union (ftv-type param) (ftv-type ret))))

(defn apply-type [s t]
  (match t
    [::t-var n] (if-let [s n] t n)
    [::t-fun t1 t2] (t-fun (apply-type s t1) (apply-type s t2))
    [_] t))


;;; instance Types Scheme
(defn apply-scheme [s [vars t]]
  (scheme vars (apply-type (apply dissoc s vars) t)))


;;; instance Types List
(defn ftv-coll [c]
  (apply set/union (map ftv-type c)))


(defn- map-values [f m]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn compose-subst [s1 s2]
  (merge (map-values (partial apply-type s1) s2) s1))

;;; instance Types type-env
(defn ftv-env [env]
  (ftv-coll (vals env)))

(defn apply-env [s env]
  (map-values (partial apply-scheme s) env))
