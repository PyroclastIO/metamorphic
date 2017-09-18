(ns metamorphic.trace
  (:require [metamorphic.runtime :as rt]
            [metamorphic.match-buffer :as m]
            [metamorphic.util :as u]))

(defn update-history [history pending-states head]
  (reduce
   (fn [history* {:state/keys [previous-id id] :as pending-state}]
     (cond-> history*
       (not (get history* previous-id))
       (assoc previous-id [{:action :start :name head}])

       true
       ((fn [h] (assoc h id (get h previous-id))))

       true
       ((fn [h] (update h id conj {:name (:pattern/name (:state/pattern pending-state))
                                  :transition (:state/last-transition pending-state)})))))
   history
   pending-states))

(defn iterate-events [runtime events head]
  (reduce
   (fn [all [event k]]
     (let [result (rt/evaluate-event (:runtime all) event {:id k})]
       (-> all
           (assoc :history (update-history (:history all) (:pending-states result) head))
           (assoc :runtime result))))
   {:runtime runtime
    :history {}}
   (map vector events (range))))

(defn trace-match-history [pattern-sequence events]
  (let [runtime (rt/initialize-runtime pattern-sequence {:trace? true})
        head (get-in pattern-sequence [:pattern-sequence/patterns 0 :pattern/name])
        result (iterate-events runtime events head)]
    (map
     (fn [match]
       (let [previous-id (:state/previous-id (meta match))]
         {:match match
          :history (conj (get (:history result) previous-id) {:action :complete})}))
     (get-in result [:runtime :matches]))))

(defn trace-partial-match-history [pattern-sequence events]
  (let [runtime (rt/initialize-runtime pattern-sequence {:trace? true})
        head (get-in pattern-sequence [:pattern-sequence/patterns 0 :pattern/name])
        result (iterate-events runtime events head)
        id->event (zipmap (range (count events)) events)]
    (reduce
     (fn [all state]
       (let [{:state/keys [last-event version]} state
             {:state/keys [persistent-id]} (meta state)
             partial-match (reverse (m/traverse-pointers last-event version))]
         (update all (get id->event persistent-id) u/conjv partial-match)))
     {}
     (get-in result [:runtime :pending-states]))))
