(ns metamorphic.util
  #?(:clj (:import [java.util UUID])))

(def final-state-name "__metamorphic-final__")

(defn conjv [xs x]
  ((fnil conj []) xs x))

(defn index-by [k xs]
  (zipmap (map k xs) xs))

(defn kw->fn [kw]
  #?(:clj
     (let [user-ns (symbol (namespace kw))
           user-fn (symbol (name kw))]
       (or (ns-resolve user-ns user-fn)
           (throw (Exception.))))
     :cljs
     (js/eval
      (str (munge-str (str (namespace kw)))
           "."
           (munge-str (str (name kw)))))))

(defn resolve-fn [f]
  (cond (keyword? f) (kw->fn f)
        (fn? f) f
        :else (throw (ex-info "Unsupported function type." {}))))

(defn invert-map-coll [m]
  (reduce-kv
   (fn [all k v]
     (update all v conjv k))
   {}
   m))

(defn select-keys-by [m f]
  (reduce-kv
   (fn [all k v]
     (if (f v)
       (assoc all k v)
       all))
   {}
   m))

(defn random-uuid []
  #?(:clj (UUID/randomUUID)
     :cljs (cljs.core/random-uuid)))
