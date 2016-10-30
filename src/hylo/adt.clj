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

(defn- -match-line [x [[v & ls] e]]
  (if (keyword? v)
    `[~v ~(if (seq ls)
            `(let [[~@ls] ~x] ~e)
            e)]
    `[(let [~v ~x] ~e)]))

(defn- -match [val lines]
  (let [v (gensym "v")]
    `(let [~v ~val]
       (case (variant ~v)
         ~@(mapcat (partial -match-line v)
                   (partition 2 lines))))))

(defmacro match [val & lines]
  (-match val lines))
