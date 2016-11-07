(ns hylo.core
  (:require [hylo.adt :refer :all]
            [hylo.types :refer :all]))

(defn new-ty-var [prefix]
  (-> (gensym (str prefix "_"))
      keyword
      t-var))

(defn instantiate [[vars t]]
  (let [nvars (map (constantly (new-ty-var "a")) vars)
        s (into {} (map vector vars nvars))]
    (apply-type s t)))

(defn generalize [env t]
  (scheme (into [] (apply disj (ftv-type t) (ftv-env env))) t))

(defn var-bind [u t]
  (cond
    (and (= (variant t) :hylo.types/t-var)
         (= t (t-var u)))
    {}

    (contains? (ftv-type t) u)
    (throw (RuntimeException.
            (str "occur check fails: " u " vs. " (show-type t))))

    :else
    {u t}))

(defn mgu [t1 t2]
  (cond
    (and (= :hylo.types/t-fun (variant t1))
         (= :hylo.types/t-fun (variant t2)))
    (let [[l1 r1] t1
          [l2 r2] t2
          s1 (mgu l1 l2)
          s2 (mgu (apply-type s1 r1) (apply-type s1 r2))]
      (compose-subst s1 s2))

    (= :hylo.types/t-var (variant t1))
    (let [[u] t1]
      (var-bind u t2))

    (= :hylo.types/t-var (variant t2))
    (let [[u] t2]
      (var-bind u t1))

    (and (= :hylo.types/t-prim (variant t1))
         (= :hylo.types/t-prim (variant t2))
         (= t1 t2))
    {}

    :else
    (throw (RuntimeException. (str "types do not unify: " t1 " vs. " t2)))))

(defn ti [env exp]
  (match exp
    [:hylo.types/e-sym n]
    (if-let [sigma (env n)]
      [{} (assoc-meta exp :ti (instantiate sigma))]
      (throw (RuntimeException. (str "unbound variable: " n))))

    [:hylo.types/e-lit t v]
    [{} (assoc-meta exp :ti (t-prim t))]

    [:hylo.types/e-fn n e]
    (let [tv (new-ty-var "b")
          env' (dissoc env n)
          env'' (merge env' {n (scheme [] tv)})
          [s1 e'] (ti env'' e)
          tf (t-fun (apply-type s1 tv)
                    (:ti (meta e')))]
      [s1 (assoc-meta (e-fn n e') :ti tf)])

    [:hylo.types/e-ap e1 e2]
    (let [tv (new-ty-var "c")
          [s1 e1'] (ti env e1)
          [s2 e2'] (ti (apply-type s1 env) e2)
          s3 (mgu (apply-type s2 (:ti (meta e1')))
                  (t-fun (:ti (meta e2')) tv))
          s4 (compose-subst (compose-subst s3 s2) s1)]
      [s4 (apply-exp s4 (assoc-meta (e-ap e1' e2') :ti (apply-type s3 tv)))])

    [:hylo.types/e-let x e1 e2]
    (let [[s1 e1'] (ti env e1)
          env' (dissoc env x)
          t' (generalize (apply-env s1 env) (:ti (meta e1')))
          env'' (assoc env' x t')
          [s2 e2'] (ti (apply-env s1 env'') e2)
          s3 (compose-subst s1 s2)
          e1'' (apply-exp s3 e1')]
      [s3 (assoc-meta (e-let x e1'' e2') :ti (:ti (meta e2')))])))

(defn type-inference [env e]
  (let [[s e] (ti env e)
        t (:ti (meta e))]
    (apply-type s t)))

(def clj->ir)

(defn- make-let [ps e]
  (if (seq ps)
    (let [[n v & rs] ps]
      (e-let n (clj->ir v) (make-let rs e)))
    (clj->ir e)))

(defn- make-fn [[p & ps] e]
  (e-fn p
        (if (seq ps)
          (make-fn ps e)
          (clj->ir e))))

(defn- make-ap [f [p & ps]]
  (if (seq ps)
    (make-ap (e-ap f (clj->ir p)) ps)
    (e-ap (clj->ir f) (clj->ir p))))

(defn clj->ir [body]
  (cond
    (instance? Boolean body)
    (e-lit :boolean body)

    (instance? Long body)
    (e-lit :long body)

    (instance? Double body)
    (e-lit :double body)

    (keyword? body)
    (e-lit :keyword body)

    (symbol? body)
    (e-sym body)

    :else
    (let [[f & xs] body]
      (condp = f
        'let (apply make-let xs)
        'fn (apply make-fn xs)
        (make-ap f xs)))))
