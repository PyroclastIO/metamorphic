(ns metamorphic.compiler
  (:require [clojure.spec.alpha :as s]
            [metamorphic.validation :as v]
            [metamorphic.util :as u]
            [metamorphic.spec]))

(defprotocol INFAState
  (states [this starting-state?])
  (add-contiguity [this states])
  (attach-begin-transitions [this states successor])
  (attach-remain-transitions [this states])
  (attach-proceed-transitions [this states successor])
  (attach-ignore-transitions [this states starting-state? successor]))

(defn ignore-followed-by-singleton? [begin-results context]
  (let [{:state/keys [last-transition last-event]} context]
    (if (= last-transition :proceed)
      (and last-event (not (seq begin-results)))
      (not (seq begin-results)))))

(defn ignore-followed-by-any-singleton? [begin-results context]
  (let [{:state/keys [last-transition last-event]} context]
    (if (= last-transition :proceed)
      (not (nil? last-event))
      true)))

(defn maybe-proceed-to-final [successor k]
  (if (= successor u/final-state-name)
    (fn [results context]
      (seq (get results k)))
    (constantly true)))

(defrecord SingletonState [pattern]
  INFAState
  (states [this starting-state?]
    (cond-> {:base {:state/name (:pattern/name pattern)
                    :state/kind :singleton
                    :state/pattern pattern}}

      (:pattern/optional? pattern)
      (assoc :escape
             {:state/name (str (:pattern/name pattern) "-escape")
              :state/kind :singleton
              :state/pattern pattern})))

  (add-contiguity [this states]
    (cond-> states
      true
      (update :base assoc :state/contiguity (:pattern/contiguity pattern))

      (:pattern/optional? pattern)
      (update :escape assoc :state/contiguity (:pattern/contiguity pattern))))

  (attach-begin-transitions [this states successor]
    (cond-> states
      true
      (update-in [:base :state/transitions] conj
                 {:transition/to successor
                  :transition/kind :begin
                  :transition/condition (u/resolve-fn (:pattern/predicate pattern))})

      (:pattern/optional? pattern)
      (update-in [:escape :state/transitions] conj
                 {:transition/to successor
                  :transition/kind :begin
                  :transition/condition (u/resolve-fn (:pattern/predicate pattern))})))

  (attach-remain-transitions [this states]
    states)

  (attach-proceed-transitions [this states successor]
    (cond-> states
      (:pattern/optional? pattern)
      (update-in [:base :state/transitions] conj
                 {:transition/to successor
                  :transition/kind :proceed
                  :transition/condition (maybe-proceed-to-final successor :begin)})))

  (attach-ignore-transitions [this states starting-state? successor]
    (let [{:keys [pattern/contiguity]} pattern]
      (if starting-state?
        states
        (if (not (:pattern/optional? pattern))
          (cond-> states
            (= contiguity :followed-by)
            (update-in [:base :state/transitions] conj
                       {:transition/to (get-in states [:base :state/name])
                        :transition/kind :ignore
                        :transition/condition
                        (fn [begin-results context]
                          (ignore-followed-by-singleton? begin-results context))})

            (= contiguity :followed-by-any)
            (update-in [:base :state/transitions] conj
                       {:transition/to (get-in states [:base :state/name])
                        :transition/kind :ignore
                        :transition/condition
                        (fn [begin-results context]
                          (ignore-followed-by-any-singleton? begin-results context))}))

          (cond-> states
            (= contiguity :followed-by)
            (update-in [:base :state/transitions] conj
                       {:transition/to (get-in states [:escape :state/name])
                        :transition/kind :ignore
                        :transition/condition
                        (fn [begin-results context]
                          (ignore-followed-by-singleton? begin-results context))})

            (= contiguity :followed-by)
            (update-in [:escape :state/transitions] conj
                       {:transition/to (get-in states [:escape :state/name])
                        :transition/kind :ignore
                        :transition/condition
                        (fn [begin-results context]
                          (not (seq begin-results)))})

            (= contiguity :followed-by-any)
            (update-in [:base :state/transitions] conj
                       {:transition/to (get-in states [:escape :state/name])
                        :transition/kind :ignore
                        :transition/condition
                        (fn [begin-results context]
                          (ignore-followed-by-any-singleton? begin-results context))})

            (= contiguity :followed-by-any)
            (update-in [:escape :state/transitions] conj
                       {:transition/to (get-in states [:escape :state/name])
                        :transition/kind :ignore
                        :transition/condition
                        (constantly true)})))))))

(defrecord LoopingState [pattern]
  INFAState
  (states [this starting-state?]
    (let [looping-contiguity (:pattern/looping-contiguity pattern :followed-by)]
      (cond-> {:base {:state/name (:pattern/name pattern)
                      :state/kind :singleton
                      :state/pattern pattern}

               :iterations {:state/name (str (:pattern/name pattern) "-iterations")
                            :state/kind :looping
                            :state/pattern pattern}}

        (and (not starting-state?)
             (:pattern/optional? pattern)
             (not= (:pattern/contiguity pattern) :next))
        (assoc :base-escape
               {:state/name (str (:pattern/name pattern) "-escape")
                :state/kind :singleton
                :state/pattern pattern})

        (not= looping-contiguity :next)
        (assoc :iterations-escape
               {:state/name (str (:pattern/name pattern) "-iterations-escape")
                :state/kind :singleton
                :state/pattern pattern}))))

  (add-contiguity [this states]
    (let [looping-contiguity (:pattern/looping-contiguity pattern :followed-by)]
      (cond-> states
        true
        (update :base assoc :state/contiguity (:pattern/contiguity pattern))

        true
        (update :iterations assoc :state/contiguity looping-contiguity)

        (:base-escape states)
        (update :base-escape assoc :state/contiguity (:pattern/contiguity pattern))

        (:iterations-escape states)
        (update :iterations-escape assoc :state/contiguity looping-contiguity))))

  (attach-begin-transitions [this states successor]
    (cond-> states
      true
      (update-in [:base :state/transitions] conj
                 {:transition/to (get-in states [:iterations :state/name])
                  :transition/kind :begin
                  :transition/condition (u/resolve-fn (:pattern/predicate pattern))})

      (and (:base-escape states) (not (:pattern/optional? pattern)))
      (update-in [:base-escape :state/transitions] conj
                 {:transition/to (get-in states [:base :state/name])
                  :transition/kind :begin
                  :transition/condition (u/resolve-fn (:pattern/predicate pattern))})

      (and (:base-escape states) (:pattern/optional? pattern))
      (update-in [:base-escape :state/transitions] conj
                 {:transition/to (get-in states [:iterations :state/name])
                  :transition/kind :begin
                  :transition/condition (u/resolve-fn (:pattern/predicate pattern))})

      (:iterations-escape states)
      (update-in [:iterations-escape :state/transitions] conj
                 {:transition/to (get-in states [:iterations :state/name])
                  :transition/kind :begin
                  :transition/condition (u/resolve-fn (:pattern/predicate pattern))})))

  (attach-remain-transitions [this states]
    (let [f (u/resolve-fn (or (:pattern/looping-predicate pattern)
                              (:pattern/predicate pattern)))]
      (update-in states [:iterations :state/transitions] conj
                 {:transition/to (get-in states [:iterations :state/name])
                  :transition/kind :remain
                  :transition/condition
                  (fn [context segment history pattern-sequence pattern]
                    (f segment history pattern-sequence pattern))})))

  (attach-proceed-transitions [this states successor]
    (cond-> states
      true
      (update-in [:iterations :state/transitions] conj
                 {:transition/to successor
                  :transition/kind :proceed
                  :transition/condition (maybe-proceed-to-final successor :remain)})
      
      (:pattern/optional? pattern)
      (update-in [:base :state/transitions] conj
                 {:transition/to successor
                  :transition/kind :proceed
                  :transition/condition (maybe-proceed-to-final successor :begin)})))

  (attach-ignore-transitions [this states starting-state? successor]
    (cond-> states
      (and (not (:pattern/optional? pattern)) (= (get-in states [:base :state/contiguity]) :followed-by))
      (update-in [:base :state/transitions] conj
                 {:transition/to (get-in states [:base :state/name])
                  :transition/kind :ignore
                  :transition/condition
                  (fn [begin-results context]
                    (ignore-followed-by-singleton? begin-results context))})

      (and (not (:pattern/optional? pattern)) (= (get-in states [:base :state/contiguity]) :followed-by-any))
      (update-in [:base :state/transitions] conj
                 {:transition/to (get-in states [:base :state/name])
                  :transition/kind :ignore
                  :transition/condition
                  (fn [begin-results context]
                    (ignore-followed-by-any-singleton? begin-results context))})

      (and (:base-escape states) (= (get-in states [:base :state/contiguity]) :followed-by))
      (update-in [:base :state/transitions] conj
                 {:transition/to (get-in states [:base-escape :state/name])
                  :transition/kind :ignore
                  :transition/condition
                  (fn [begin-results context]
                    (and (not (seq begin-results))
                         (not (nil? (:state/last-event context)))))})

      (and (:base-escape states) (= (get-in states [:base :state/contiguity]) :followed-by-any))
      (update-in [:base :state/transitions] conj
                 {:transition/to (get-in states [:base-escape :state/name])
                  :transition/kind :ignore
                  :transition/condition
                  (fn [begin-results context]
                    (not (nil? (:state/last-event context))))})

      (and (:base-escape states) (= (get-in states [:base-escape :state/contiguity]) :followed-by))
      (update-in [:base-escape :state/transitions] conj
                 {:transition/to (get-in states [:base-escape :state/name])
                  :transition/kind :ignore
                  :transition/condition
                  (fn [begin-results context]
                    (not (seq begin-results)))})

      (and (:base-escape states) (= (get-in states [:base-escape :state/contiguity]) :followed-by-any))
      (update-in [:base-escape :state/transitions] conj
                 {:transition/to (get-in states [:base-escape :state/name])
                  :transition/kind :ignore
                  :transition/condition (constantly true)})

      (and (:iterations-escape states) (= (get-in states [:iterations :state/contiguity]) :followed-by))
      (update-in [:iterations :state/transitions] conj
                 {:transition/to (get-in states [:iterations-escape :state/name])
                  :transition/kind :ignore
                  :transition/condition
                  (fn [remain-results context]
                    (not (seq remain-results)))})

      (and (:iterations-escape states) (= (get-in states [:iterations :state/contiguity]) :followed-by-any))
      (update-in [:iterations :state/transitions] conj
                 {:transition/to (get-in states [:iterations-escape :state/name])
                  :transition/kind :ignore
                  :transition/condition (constantly true)})

      (and (:iterations-escape states) (= (get-in states [:iterations-escape :state/contiguity]) :followed-by))
      (update-in [:iterations-escape :state/transitions] conj
                 {:transition/to (get-in states [:iterations-escape :state/name])
                  :transition/kind :ignore
                  :transition/condition
                  (fn [begin-results context]
                    (not (seq begin-results)))})

      (and (:iterations-escape states) (= (get-in states [:iterations-escape :state/contiguity]) :followed-by-any))
      (update-in [:iterations-escape :state/transitions] conj
                 {:transition/to (get-in states [:iterations-escape :state/name])
                  :transition/kind :ignore
                  :transition/condition (constantly true)}))))

(defn build-records [patterns]
  (reduce
   (fn [all pattern]
     (let [record (if (:pattern/repetition pattern)
                    (->LoopingState pattern)
                    (->SingletonState pattern))]
       (assoc all (:pattern/name pattern) {:record record})))
   {}
   patterns))

(defn build-base-states [context patterns]
  (reduce
   (fn [all [state-name i]]
     (let [{:keys [record]} (get all state-name)
           starting-state? (zero? i)]
       (-> all
           (assoc-in [state-name :states] (states record starting-state?))
           (assoc-in [state-name :starting-state?] starting-state?))))
   context
   (map vector (map :pattern/name patterns) (range))))

(defn build-state-contiguity [context patterns]
  (reduce
   (fn [all state-name]
     (let [{:keys [record states]} (get all state-name)
           reified-states (add-contiguity record states)]
       (assoc-in all [state-name :states] reified-states)))
   context
   (map :pattern/name patterns)))

(defn add-successor [context patterns]
  (reduce
   (fn [all [state successor]]
     (let [successor (or (:pattern/name successor) u/final-state-name)]
       (assoc-in all [(:pattern/name state) :successor] successor)))
   context
   (partition-all 2 1 patterns)))

(defn add-begin-transitions [context patterns]
  (reduce
   (fn [all state-name]
     (let [{:keys [record states successor]} (get all state-name)
           reified-states (attach-begin-transitions record states successor)]
       (assoc-in all [state-name :states] reified-states)))
   context
   (map :pattern/name patterns)))

(defn add-remain-transitions [context patterns]
  (reduce
   (fn [all state-name]
     (let [{:keys [record states]} (get all state-name)
           reified-states (attach-remain-transitions record states)]
       (assoc-in all [state-name :states] reified-states)))
   context
   (map :pattern/name patterns)))

(defn add-proceed-transitions [context patterns]
  (reduce
   (fn [all state-name]
     (let [{:keys [record states successor]} (get all state-name)
           reified-states (attach-proceed-transitions record states successor)]
       (assoc-in all [state-name :states] reified-states)))
   context
   (map :pattern/name patterns)))

(defn add-ignore-transitions [context patterns]
  (reduce
   (fn [all [state-name i]]
     (let [{:keys [record states starting-state? successor]} (get all state-name)
           reified-states (attach-ignore-transitions record states starting-state? successor)]
       (assoc-in all [state-name :states] reified-states)))
   context
   (map vector (map :pattern/name patterns) (range))))

(defn compute-acceleration [transitions successor]
  (let [targets (map (juxt :transition/to :transition/kind) transitions)]
    (boolean (some #{[u/final-state-name :proceed]} targets))))

(defn group-transitions [context patterns]
  (reduce
   (fn [all state-name]
     (let [{:keys [record states successor]} (get all state-name)
           reified-states
           (map
            (fn [state]
              (let [acceleration (compute-acceleration (:state/transitions state) successor)]
                (-> state
                    (assoc :state/accelerate? acceleration)
                    (update :state/transitions (partial group-by :transition/kind)))))
            (vals states))]
       (assoc-in all [state-name :states] reified-states)))
   context
   (map :pattern/name patterns)))

(defn add-final-state [context]
  (let [state {:state/name u/final-state-name
               :state/kind :final}]
    (assoc-in context [(name u/final-state-name) :states] [state])))

(defn index-states [context]
  (reduce-kv
   (fn [all pattern-name {:keys [states]}]
     (reduce
      (fn [all* state]
        (assoc all* (:state/name state) state))
      all
      states))
   {}
   context))

(defn compile-nfa [{:keys [pattern-sequence/patterns pattern-sequence/within] :as ps}]
  (when-let [ed (s/explain-data :cep/pattern-sequence ps)]
    (throw (ex-info "Pattern definition failed to conform to spec." {:error-data ed})))

  (v/validate-pattern-sequence! ps)

  (let [initial-state (:pattern/name (first patterns))]
    {:states (-> patterns
                 (build-records)
                 (build-base-states patterns)
                 (build-state-contiguity patterns)
                 (add-successor patterns)
                 (add-begin-transitions patterns)
                 (add-remain-transitions patterns)
                 (add-proceed-transitions patterns)
                 (add-ignore-transitions patterns)
                 (group-transitions patterns)
                 (add-final-state)
                 (index-states))
     :initial-state initial-state
     :current-state initial-state
     :match-period within}))
