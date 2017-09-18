(ns metamorphic.api
  (:refer-clojure :exclude [next])
  (:require [metamorphic.util :as u]))

(defn ^{:no-doc true} base-pattern [pattern-name pred contiguity]
  {:pattern/name pattern-name
   :pattern/kind :singleton
   :pattern/predicate pred
   :pattern/contiguity contiguity})

(defn ^{:no-doc true} attach-options [pattern opts]
  (cond-> pattern
    (:optional? opts)
    (assoc :pattern/optional? true)

    (:consecutive? opts)
    (assoc :pattern/looping-contiguity :next)

    (:allow-combinations? opts)
    (assoc :pattern/looping-contiguity :followed-by-any)

    (:repetition opts)
    ((fn [p]
       (-> p
           (assoc :pattern/kind :looping)
           (assoc :pattern/repetition (:repetition opts)))))

    (:looping-predicate opts)
    (assoc :pattern/looping-predicate (:looping-predicate opts))))

(defn new-pattern-sequence
  "Creates a new pattern sequence with a name. Optionally supply:

   - :within - The match must fully occur within N milliseconds."
  ([sequence-name]
   (new-pattern-sequence sequence-name nil))
  ([sequence-name opts]
   (cond-> {:pattern-sequence/name sequence-name
            :pattern-sequence/patterns []}
     (:within opts)
     (assoc :pattern-sequence/within (:within opts)))))

(defn begin
  "Starts a new pattern sequence. All pattern sequences must start with
   a begin state. Selects the first relevent event to initiate a pattern match."
  ([context pattern-name pred]
   (begin context pattern-name pred {}))
  ([context pattern-name pred opts]
   (update context :pattern-sequence/patterns u/conjv
           (-> pattern-name
               (base-pattern pred :next)
               (attach-options opts)))))

(defn next
  "Strict contiguity event selection. Enforces this pattern match to occur immediately
   after the previous match occured."
  ([context pattern-name pred]
   (next context pattern-name pred {}))
  ([context pattern-name pred opts]
   (update context :pattern-sequence/patterns u/conjv
           (-> pattern-name
               (base-pattern pred :next)
               (attach-options opts)))))

(defn followed-by
  "Relaxed contiguity event selection. Allows this pattern match to tolerate
   irrelevant events before matching."
  ([context pattern-name pred]
   (followed-by context pattern-name pred {}))
  ([context pattern-name pred opts]
   (update context :pattern-sequence/patterns u/conjv
           (-> pattern-name
               (base-pattern pred :followed-by)
               (attach-options opts)))))

(defn followed-by-any
  "Nondeterministic relaxed contiguity event selection. Allows this pattern match to tolerate
   irrelevant events before matching, and also continues to match from this point
   after each match occurs."
  ([context pattern-name pred]
   (followed-by-any context pattern-name pred {}))
  ([context pattern-name pred opts]
   (update context :pattern-sequence/patterns u/conjv
           (-> pattern-name
               (base-pattern pred :followed-by-any)
               (attach-options opts)))))
