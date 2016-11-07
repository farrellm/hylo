(ns hylo.types
  (:require [hylo.adt :refer :all]
            [clojure.set :as set]))

(defn assoc-meta [obj k v]
  (with-meta obj (assoc (meta obj) k v)))

(defn update-meta [obj k f]
  (with-meta obj (update (meta obj) k f)))


(defadt exp
  (e-sym name)
  (e-lit type val)
  (e-ap func arg)
  (e-fn param exp)
  (e-let name val exp))

(defadt type
  (t-var name)
  (t-prim type)
  (t-fun param ret))

(defadt scheme
  (scheme ns ty))


;;; instance Types Type
(defn ftv-type [t]
  (match t
    [::t-var n] #{n}
    [::t-prim _] #{}
    [::t-fun param ret] (set/union (ftv-type param) (ftv-type ret))))

(defn apply-type [s t]
  (match t
    [::t-var n] (if-let [t' (s n)] t' t)
    [::t-fun t1 t2] (t-fun (apply-type s t1) (apply-type s t2))
    [_] t))


;;; instance Types Scheme
(defn ftv-scheme [s]
  (match s
    [::scheme vs t] (set/difference (ftv-type t) (into #{} vs))))

(defn apply-scheme [s [vars t]]
  (scheme vars (apply-type (apply dissoc s vars) t)))


;;; instance Types List
(defn ftv-coll-type [c]
  (apply set/union (map ftv-type c)))

(defn ftv-coll-scheme [c]
  (apply set/union (map ftv-scheme c)))


(defn- map-values [f m]
  (into {} (map (fn [[k v]] [k (f v)]) m)))

(defn compose-subst [s1 s2]
  (merge (map-values (partial apply-type s1) s2) s1))

;;; instance Types type-env
(defn ftv-env [env]
  (ftv-coll-scheme (vals env)))

(defn apply-env [s env]
  (map-values (partial apply-scheme s) env))



;;; instance Types Exp
(defn apply-exp [s exp]
  (when (-> (meta exp) (:ti) nil?)
    (throw (RuntimeException. (str "Type inference missing on " exp))))
  (match exp
    [::e-sym n]
    (update-meta exp :ti (partial apply-type s))

    [::e-lit _ _]
    exp

    [::e-ap f a]
    (assoc-meta (e-ap (apply-exp s f)
                      (apply-exp s a))
                :ti (apply-type s (:ti (meta exp))))

    [::e-fn p e]
    (assoc-meta (e-fn p (apply-exp s e))
                :ti (apply-type s (:ti (meta exp))))

    [::e-let n v e]
    (assoc-meta (e-let n (apply-exp s v)
                       (apply-exp s e))
                :ti (apply-type s (:ti (meta exp))))))


(defn show-type [t]
  (match t
    [:hylo.types/t-prim t] (str t)
    [:hylo.types/t-var n] n
    [:hylo.types/t-fun t1 t2] (str (show-type t1) " -> " (show-type t2))))

(defn show-exp [exp]
  (match exp
    [:hylo.types/e-var n] (str n)
    [:hylo.types/e-lit _ l] (str l)
    [:hylo.types/e-fn p e] (str "(fn [" (show-exp p) "]" (show-exp e) ")")
    [:hylo.types/e-ap f a] (str "(" (show-exp f) " " (show-exp a) ")")))
