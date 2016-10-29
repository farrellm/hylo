(ns hylo.core
  (:require [hylo.adt :refer :all]
            [hylo.types :refer :all]))

(defadt maybe
  nothing
  (just value))

(defn new-ty-var [prefix]
  (t-var (gensym prefix)))

(defn instantiate [[vars t]]
  (let [nvars (map (constantly (new-ty-var "a")) vars)
        s (into {} (map vector vars nvars))]
    (apply-types s t)))

(defn generalize [env t]
  (scheme (into [] (apply disj (ftv t) (ftv env))) t))

(defn var-bind [u t]
  (cond
    (= t (t-var)) null-subst
    (contains? (ftv t) u) (throw (RuntimeException.
                                  (str "occur check fails: " u " vs. " t)))
    :else
    {u t}))

(defn mgu [t1 t2]
  (cond
    (and (= :hylo.types/t-fun (variant t1))
         (= :hylo.types/t-fun (variant t1)))
    (let [[l1 r1] t1
          [l2 r2] t2
          s1 (mgu l1 l2)
          s2 (mgu (apply-types s1 r1) (apply-types s1 r2))]
      (compose-subst s1 s2))

    (= :hylo.types/t-var (variant t1))
    (let [[u] t1]
      (var-bind u t2))

    (= :hylo.types/t-var (variant t2))
    (let [[u] t2]
      (var-bind u t1))

    (and (= :hylo.types/t-int (variant t1))
         (= :hylo.types/t-int (variant t2)))
    null-subst

    (and (= :hylo.types/t-bool (variant t1))
         (= :hylo.types/t-bool (variant t2)))
    null-subst

    :else
    (throw (RuntimeException. (str "types do not unify: " t1 " vs. " t2)))))

(defn ti-lit [lit]
  (case (variant lit)
    :hylo.types/l-int [null-subst t-int]
    :hylo.types/l-bool [null-subst t-bool]))

(defn ti [env exp]
  (case (variant exp)
    :hylo.types/e-var
    (let [[n] exp]
      (if-let [sigma (env n)]
        [null-subst (instantiate sigma)]
        (throw (RuntimeException. (str "unbound variable: " n)))))

    :hylo.types/e-lit
    (let [[l] exp]
      (ti-lit l))

    :hylo.types/e-abs
    (let [[n e] exp
          tv (new-ty-var "b")
          env' (dissoc env n)
          env'' (merge env' {n (scheme [] tv)})
          [s1 t1] (ti env'' e)]
      [s1 (t-fun (apply-types s1 tv) t1)])

    :hylo.types/e-app
    (let [[e1 e2] exp
          tv (new-ty-var "c")
          [s1 t1] (ti env e1)
          [s2 t2] (ti (apply-types s1 env) e2)
          s3 (mgu (apply-types s2 t1) (t-fun t2 tv))]
      [(compose-subst (compose-subst s3 s2) s1) (apply-types s3 tv)])

    :hylo.types/e-let
    (let [[x e1 e2] exp
          [s1 t1] (ti env e1)
          env' (dissoc env x)
          t' (generalize (apply-types s1 env) t1)
          env'' (assoc env' x t')
          [s2 t2] (ti (apply-types s1 env'') e2)]
      [(compose-subst s1 s2) t2])
    ))

(defn type-inference [env e]
  (let [[s t] (ti env e)]
    (apply-types s t)))

(defn show-type [t]
  (case (variant t)
    :hylo.types/t-bool "Bool"
    :hylo.types/t-int "Int"
    :hylo.types/t-var (let [[n] t] n)
    :hylo.types/t-fun (let [[t1 t2] t] (str t1 " -> " t2))))
