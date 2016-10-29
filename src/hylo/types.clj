(ns hylo.types
  (:require [hylo.adt :refer :all]
            [clojure.set :as set]))

(derive clojure.lang.PersistentVector ::list)
(derive clojure.lang.ISeq ::list)
(derive clojure.lang.PersistentArrayMap ::map)
(derive clojure.lang.PersistentHashSet ::set)

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


;;; class Types
(def ftv nil)
(defmulti ftv type)
(def apply-types nil)
(defmulti apply-types (fn [_ v] (type v)))

;;; instance Types Type
(defmethod ftv ::type [t]
  (case (variant t)
    ::t-var (let [[n] t] #{n})
    ::t-int #{}
    ::t-bool #{}
    ::t-fun (let [[param ret] t]
              (set/union (ftv param) (ftv ret)))))

(defmethod apply-types ::type [s t]
  (case (variant t)
    ::t-var (let [[n] t]
              (if-let [s n] t n))
    ::t-fun (let [[t1 t2] t]
              (t-fun (apply-types s t1) (apply-types s t2)))
    t))


;;; instance Types Scheme
(defmethod ftv ::scheme [[vars t]]
  (set/difference (ftv t) (into #{} vars)))

(defmethod apply-types ::scheme [s [vars t]]
  (scheme vars (apply-types (apply dissoc s vars))))


;;; instance Types List
(defmethod ftv ::list [l]
  (apply set/union (map ftv l)))
(defmethod ftv nil [_]
  (ftv []))

(defmethod apply-types ::list [s l]
  (map (partial apply-types s) l))
(defmethod apply-types nil [s _]
  (map (partial apply-types s) []))


(def null-subst {})

(defn- map-values [f m]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn compose-subst [s1 s2]
  (merge (map-values (partial apply-types s1) s2) s1))

;; (defadt type-env
;;   (type-env env))

;; (defn type-env-dissoc [[env] var]
;;   (dissoc env var))

(def empty-env ^{:type ::type-env} {})

;;; instance Types type-env
(defmethod ftv ::type-env [env]
  (ftv (vals env)))

(defmethod apply-types ::type-env [s env]
  (map-values (partial apply-types s) env))
