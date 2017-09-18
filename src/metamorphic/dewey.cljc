(ns metamorphic.dewey
  (:require [metamorphic.util :refer [conjv]]))

(defn new-dewey-number
  ([]
   [])
  ([start]
   [start]))

(defn increase
  ([dn]
   (increase dn 1))
  ([dn k]
   (assert (pos? (count dn)) "Dewey number was empty.")
   (update dn (dec (count dn)) + k)))

(defn add-stage
  ([dn]
   (conjv dn 0))
  ([dn k]
   (conjv dn k)))

(defn compatible? [a b]
  (cond (> (count a) (count b))
        (= (take (count b) a) b)

        (= (count a) (count b))
        (and (= (butlast a) (butlast b))
             (>= (last a) (last b)))

        :else false))
