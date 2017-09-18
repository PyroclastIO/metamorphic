(ns metamorphic.match-buffer
  (:require [metamorphic.dewey :as d]
            [metamorphic.util :as u]
            [metamorphic.compiler :as c]
            #?(:cljs [goog.string :refer [format]])))

(defprotocol IPointer
  (inc-refs! [this stage n])
  (dec-refs! [this stage])
  (ref-count [this stage])
  (referenced? [this]))

(defprotocol ITraverse
  (text-history [this]))

(defrecord Event [payload state-name pattern-name pointers refs]
  IPointer
  (inc-refs! [this stage n]
    (assert (pos? n))
    (swap! refs update stage (fnil + 0) n))

  (dec-refs! [this stage]
    (swap! refs update stage
           (fn [k]
             (let [n (dec (or k 0))]
               (if (neg? n) 0 n)))))

  (ref-count [this stage]
    (get @refs stage))

  (referenced? [this]
    (not (zero? (reduce + 0 (vals @refs)))))
  
  ITraverse
  (text-history [this]
    (reduce-kv
     (fn [all pointer event]
       (let [line (format "%s => [%s] %s" pointer (.-pattern-name event) (:payload event))]
         (u/conjv all (str "\t" line))))
     [(format "[%s] %s: " pattern-name payload)]
     pointers)))

(defprotocol IStage
  (append [this event])
  (delete [this event])
  (events [this]))

(defrecord MatchBufferStage [events]
  IStage
  (append [this event]
    (assoc this :events (conj events event)))

  (delete [this event]
    (update this :events disj event))

  (events [this]
    events)

  Object
  (toString [this]
    (.toString events)))

(defn event [payload state-name pattern-name pointers]
  (Event. payload state-name pattern-name pointers (atom {})))

(defn new-match-buffer [nfa]
  (let [states (disj (into #{} (keys (:states nfa))) u/final-state-name)]
    (reduce
     (fn [all state]
       (assoc all state (MatchBufferStage. #{})))
     {}
     states)))

(defn traverse-pointers
  "Traverses the pointers to obtain a linear event history."
  ([e dn]
   (traverse-pointers e dn []))
  ([e dn result]
   (if e
     (let [result* (conj result (:payload e))
           pointers (keys (:pointers e))
           parent (first (into (vec (filter (fn [p] (= dn p)) pointers))
                               (vec (filter (fn [p] (d/compatible? dn p)) pointers))))]
       (if parent
         (recur (get (:pointers e) parent) parent result*)
         result*))
     result)))

(defn build-match-history
  "Traverses pointers to obtain a map of event history keyed by pattern name."
  ([e dn]
   (build-match-history e dn {}))
  ([e dn result]
   (if e
     (let [result* (update result (.-pattern-name e) u/conjv (.-payload e))
           pointers (keys (:pointers e))
           parent (first (filter (fn [p] (d/compatible? dn p)) pointers))]
       (if parent
         (recur (get (:pointers e) parent) parent result*)
         result*))
     result)))

(defn maybe-delete [mb event children]
  (if (and (not (referenced? event))
           (every? zero? (map (fn [e] (ref-count e (:state-name event))) children)))
    (update mb (:state-name event) delete event)
    mb))

(defn prune-match [match-buffer event pointer]
  (if (nil? event)
    match-buffer
    (let [{:keys [pointers]} event]
      (if-let [next-pointer (first (filter (fn [p] (d/compatible? pointer p)) (keys pointers)))]
        (let [child (get-in event [:pointers next-pointer])]
          (dec-refs! child (:state-name event))
          (recur (maybe-delete match-buffer event (vals pointers))
                 child
                 next-pointer))
        (maybe-delete match-buffer event (vals pointers))))))
