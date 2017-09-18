(ns metamorphic.viz
  (:require [metamorphic.util :as u]
            [rhizome.viz :as viz]))

(defn visualize-match-buffer [match-buffer]
  (viz/view-graph
   (mapcat (fn [stage] (:events stage)) (vals match-buffer))
   (fn [event] (distinct (vals (:pointers event))))
   :node->descriptor (fn [event] {:label (format "%s @%s" (:payload event) (pr-str @(:refs event)))})
   :edge->descriptor
   (fn [src dst]
     (let [xs (get (u/invert-map-coll (:pointers src)) dst)]
       {:label (pr-str (if (<= (count xs) 1) (first xs) xs))}))
   :node->cluster (fn [event] (:state-name event))
   :cluster->descriptor (fn [cluster] {:label cluster})
   :options {:dpi 55}))

(defn visualize-nfa [nfa]
  (viz/view-graph
   (keys (:states nfa))
   (fn [state]
     (->> (vals (get-in nfa [:states state :state/transitions]))
          (reduce into [])
          (map :transition/to)
          (into #{})))
   :node->descriptor (fn [state] {:label state})
   :edge->descriptor
   (fn [src dst]
     (let [transitions (reduce into [] (vals (get-in nfa [:states src :state/transitions])))
           groups (group-by :transition/to transitions)]
       {:label (pr-str (map :transition/kind (get groups dst)))}))
   :options {:dpi 60}))
