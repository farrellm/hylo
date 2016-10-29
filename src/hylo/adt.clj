(ns hylo.adt)

(defn variant [v]
  (if (:type (meta v))
    (:variant (meta v))
    (condp instance? v
      java.util.List (if (empty? v) :Nil :Cons)
      nil)))

(defn- make-cons [name elem]
  (if (symbol? elem)
    `(def ~elem (with-meta [] {:type ~(keyword (str *ns*) (str name))
                               :variant ~(keyword (str *ns*) (str elem))}))
    (let [[variant & params] elem]
      `(defn ~variant [~@params]
         (with-meta [~@params]
           {:type ~(keyword (str *ns*) (str name))
            :variant ~(keyword (str *ns*) (str variant))})))))

(defn- -defadt [name elems]
  `(do
     ~@(map (partial make-cons name) elems)))

(defmacro defadt [name & elems]
  (-defadt name elems))
