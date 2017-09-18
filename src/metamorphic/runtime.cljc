(ns metamorphic.runtime
  (:require [clojure.spec.alpha :as s]
            [metamorphic.compiler :as c]
            [metamorphic.util :as u]
            [metamorphic.dewey :as d]
            [metamorphic.match-buffer :as m]))

(defn reset-transition-count [state]
  (-> state
      (assoc :state/n-outgoing-begins 0)
      (assoc :state/n-outgoing-remains 0)
      (assoc :state/n-outgoing-proceeds 0)
      (assoc :state/n-outgoing-ignores 0)))

(defn update-transition-count [old-state new-state transition-kind]
  (let [ks [:state/n-outgoing-begins
            :state/n-outgoing-remains
            :state/n-outgoing-proceeds
            :state/n-outgoing-ignores]
        transitions (select-keys old-state ks)
        updated-transitions
        (case transition-kind
          :begin (update transitions :state/n-outgoing-begins inc)
          :remain (update transitions :state/n-outgoing-remains inc)
          :proceed (update transitions :state/n-outgoing-proceeds inc)
          :ignore (update transitions :state/n-outgoing-ignores inc))]
    (merge new-state updated-transitions)))

(defn get-transitions [rt state-name]
  (get-in rt [:nfa :states state-name :state/transitions]))

(defn prune-timed-out-runs [rt event-time]
  (let [match-period (get-in rt [:nfa :match-period])]
    (if (and match-period event-time)
      (reduce
       (fn [rt* pending-state]
         (if (> (- event-time (:state/init-time pending-state)) match-period)
           (if-let [tail (:state/last-event pending-state)]
             (let [partial-match (m/traverse-pointers tail (:state/version pending-state))]
               (-> rt*
                   (update-in [:timed-out-matches] conj (reverse partial-match))
                   (update-in [:match-buffer] m/prune-match tail (:state/version pending-state))))
             rt*)
           (update-in rt* [:pending-states] conj pending-state)))
       (assoc-in rt [:pending-states] [])
       (get-in rt [:pending-states]))
      rt)))

(defmulti evaluate-transitions
  (fn [rt state segment]
    [(:state/kind state) (:state/contiguity state)]))

(defn evaluate-singleton-transitions
  [{:keys [pattern-sequence] :as rt} {:state/keys [pattern last-transition last-event version] :as state} segment]
  (let [transitions (get-transitions rt (:state/name state))
        {:keys [begin ignore proceed]} transitions
        context {:state/last-transition last-transition
                 :state/last-event last-event}
        history (delay (m/build-match-history last-event version))
        begin-transitions
        (filter
         (fn [t]
           ((:transition/condition t) segment history pattern-sequence pattern))
         begin)
        ignore-transitions
        (filter
         (fn [t]
           ((:transition/condition t) begin-transitions context))
         ignore)
        proceed-transitions
        (filter
         (fn [t]
           ((:transition/condition t)
            {:begin begin-transitions
             :ignore ignore-transitions}
            context))
         proceed)]
    (reduce into [] [begin-transitions ignore-transitions proceed-transitions])))

(defn evaluate-looping-transitions
  [{:keys [pattern-sequence] :as rt} {:state/keys [pattern last-transition last-event version] :as state} segment]
  (let [transitions (get-transitions rt (:state/name state))
        {:keys [remain ignore proceed]} transitions
        context {:state/last-transition last-transition
                 :state/last-event last-event}
        history (delay (m/build-match-history last-event version))
        remain-transitions
        (filter
         (fn [t]
           ((:transition/condition t) context segment history pattern-sequence pattern))
         remain)
        ignore-transitions
        (filter
         (fn [t]
           ((:transition/condition t) remain-transitions context))
         ignore)
        proceed-transitions
        (filter
         (fn [t]
           ((:transition/condition t)
            {:remain remain-transitions
             :ignore ignore-transitions}
            context))
         proceed)]
    (reduce into [] [remain-transitions ignore-transitions proceed-transitions])))

(defmethod evaluate-transitions [:singleton :next]
  [rt state segment]
  (evaluate-singleton-transitions rt state segment))

(defmethod evaluate-transitions [:singleton :followed-by]
  [rt state segment]
  (evaluate-singleton-transitions rt state segment))

(defmethod evaluate-transitions [:singleton :followed-by-any]
  [rt state segment]
  (evaluate-singleton-transitions rt state segment))

(defmethod evaluate-transitions [:looping :next]
  [rt state segment]
  (evaluate-looping-transitions rt state segment))

(defmethod evaluate-transitions [:looping :followed-by]
  [rt state segment]
  (evaluate-looping-transitions rt state segment))

(defmethod evaluate-transitions [:looping :followed-by-any]
  [rt state segment]
  (evaluate-looping-transitions rt state segment))

(defn branching? [transitions]
  (let [to (map :transition/to transitions)]
    (apply not= to)))

(defn build-manifests [rt state segment]
  (let [transitions (evaluate-transitions rt state segment)]
    (when (seq transitions)
      (let [split? (branching? transitions)]
        (map
         (fn [t]
           {:manifest/src-state state
            :manifest/transition t
            :manifest/branching? split?})
         transitions)))))

(defn edge-phase [state]
  (max
   0
   (+ (max 0 (dec (:state/n-outgoing-begins state)))
      (:state/n-outgoing-remains state)
      (:state/n-outgoing-ignores state))))

(defprotocol ITransition
  (next-state [this rt])
  (next-manifests [this rt segment])
  (next-version [this rt dst-state]))

(defrecord BeginTransition [manifest]
  ITransition
  (next-state [this rt]
    (let [{:manifest/keys [src-state transition]} manifest
          dst-state (get-in rt [:nfa :states (:transition/to transition)])]
      (update-transition-count src-state dst-state :begin)))
  
  (next-manifests [this rt segment]
    [])

  (next-version [this rt dst-state]
    (let [{:manifest/keys [src-state transition]} manifest]
      (if (= (:state/name src-state) (get-in rt [:nfa :initial-state]))
        (:state/version src-state)
        (let [phase (edge-phase dst-state)]
          (-> (:state/version src-state)
              (d/add-stage)
              (d/increase phase)))))))

(defrecord RemainTransition [manifest]
  ITransition
  (next-state [this rt]
    (let [{:manifest/keys [src-state transition]} manifest
          dst-state (get-in rt [:nfa :states (:transition/to transition)])]
      (update-transition-count src-state dst-state :remain)))
  
  (next-manifests [this rt segment]
    [])

  (next-version [this rt dst-state]
    (let [{:manifest/keys [src-state branching?]} manifest]
      (cond (or branching? (not= (:state/last-transition src-state) :remain))
            (let [phase (edge-phase dst-state)]
              (d/increase (d/add-stage (:state/version src-state)) phase))

            branching?
            (let [phase (edge-phase dst-state)]
              (d/increase (:state/version src-state) phase))

            :else
            (:state/version src-state)))))

(defrecord IgnoreTransition [manifest]
  ITransition
  (next-state [this rt]
    (let [{:manifest/keys [src-state transition]} manifest
          dst-state (get-in rt [:nfa :states (:transition/to transition)])
          chained-event (:state/last-event src-state)
          with-event (assoc dst-state :state/last-event chained-event)]
      (update-transition-count src-state with-event :ignore)))

  (next-manifests [this rt segment]
    [])

  (next-version [this rt dst-state]
    (get-in manifest [:manifest/src-state :state/version])))

(defrecord ProceedTransition [manifest]
  ITransition
  (next-state [this rt]
    (let [{:manifest/keys [src-state transition]} manifest
          dst-state (get-in rt [:nfa :states (:transition/to transition)])]
      (when (= (:state/name dst-state) u/final-state-name)
        (let [chained-event (:state/last-event src-state)]
          (-> src-state
              (update-transition-count dst-state :proceed)
              (assoc :state/last-event chained-event))))))

  (next-manifests [this rt segment]
    (let [{:manifest/keys [src-state transition]} manifest
          next-state (get-in rt [:nfa :states (:transition/to transition)])]
      (when (not= (:state/name next-state) u/final-state-name)
        (let [chained-version (d/add-stage (:state/version src-state))
              chained-event (:state/last-event src-state)
              chained-state (-> next-state
                                (assoc :state/version chained-version)
                                (assoc :state/last-event chained-event)
                                (assoc :state/last-transition :proceed))
              dst-state (update-transition-count src-state chained-state :proceed)]
          (build-manifests rt dst-state segment)))))

  (next-version [this rt dst-state]
    (let [{:manifest/keys [src-state]} manifest]
      (cond (and (= (:state/name dst-state) u/final-state-name)
                 (= (:state/last-transition src-state) :remain))
            (d/increase (:state/version src-state))

            :else
            (d/add-stage (:state/version src-state))))))

(defn new-instance [nfa]
  {:match-buffer (m/new-match-buffer nfa)
   :next-root-version 1
   :pending-states []})

(defn initialize-runtime
  "Creates a new instance of the runtime for a pattern sequence.
   The runtime *is not* functionally pure and thus should not be shared
   across threads."
  ([pattern-sequence]
   (initialize-runtime pattern-sequence {}))
  ([pattern-sequence opts]
   (let [nfa (c/compile-nfa pattern-sequence)]
     (merge
      opts
      {:pattern-sequence pattern-sequence
       :nfa nfa
       :match-buffer (m/new-match-buffer nfa)
       :next-root-version 1
       :pending-states []
       :matches []
       :timed-out-matches []}))))

(defn initialize-transient-values [rt]
  (-> rt
      (assoc :processing-queue [])
      (assoc :patches [])
      (assoc :pointers {})
      (assoc :event-mapping {})
      (assoc :iteration-matches [])))

(defn add-new-starting-state [rt event-time id]
  (s/assert :cep/runtime-iteration rt)
  (let [initial-state (get-in rt [:nfa :initial-state])
        state (get-in rt [:nfa :states initial-state])
        reified-state
        (cond-> state
          true (dissoc :state/transitions)
          true (assoc :state/version (d/new-dewey-number (get-in rt [:next-root-version])))
          true (reset-transition-count)
          event-time (assoc :state/init-time event-time)
          (:trace? rt) (assoc :state/id (u/random-uuid))
          (:trace? rt) (with-meta {:state/persistent-id id}))]
    (-> rt
        (update-in [:pending-states] conj reified-state)
        (update-in [:next-root-version] inc))))

(defn load-processing-queue [rt segment]
  (s/assert :cep/runtime-iteration rt)
  (let [queue
        (reduce
         (fn [all state]
           (let [manifests (build-manifests rt state segment)]
             (into all manifests)))
         (get-in rt [:processing-queue])
         (get-in rt [:pending-states]))]
    (assoc-in rt [:processing-queue] queue)))

(defn build-transition-record [manifest]
  (case (get-in manifest [:manifest/transition :transition/kind])
    :begin (->BeginTransition manifest)
    :remain (->RemainTransition manifest)
    :proceed (->ProceedTransition manifest)
    :ignore (->IgnoreTransition manifest)))

(defn evaluate-processing-queue [{:keys [nfa processing-queue patches] :as rt} segment]
  (s/assert :cep/runtime-iteration rt)
  (loop [manifests processing-queue
         patches []]
    (if (not (seq manifests))
      (-> rt
          (assoc-in [:patches] patches)
          (assoc-in [:processing-queue] manifests))
      (let [next-manifest (first manifests)
            transition-record (build-transition-record next-manifest)
            dst-state (next-state transition-record rt)
            follow-on-manifests (next-manifests transition-record rt segment)]
        (recur (if follow-on-manifests
                 (into (rest manifests) follow-on-manifests)
                 (rest manifests))
               (if dst-state
                 (let [version (next-version transition-record rt dst-state)]
                   (conj patches
                         (-> next-manifest
                             (assoc :manifest/dst-state dst-state)
                             (assoc :manifest/version version))))
                 patches))))))

(defn collapse-pointers [rt]
  (s/assert :cep/runtime-iteration rt)
  (let [initial (get-in rt [:nfa :initial-state])]
    (reduce
     (fn [rt* {:keys [manifest/src-state manifest/transition manifest/version]}]
       (let [state-name (:state/name src-state)]
         (cond (= (:transition/kind transition) :ignore)
               rt*

               (not (:state/last-event src-state))
               (update-in rt* [:pointers state-name] (fn [pointers] (or pointers {})))

               :else
               (let [target-event (:state/last-event src-state)]
                 (when-let [e (get-in rt* [:pointers state-name version :target-event])]
                   (assert (= e target-event) (str "Pointers collapsed incorrectly, conflict at: " state-name " / " version ".")))

                 (-> rt*
                     (assoc-in [:pointers state-name version :target-event] target-event)
                     (update-in [:pointers state-name version :n] (fn [n] ((fnil inc 0) n))))))))
     rt
     (get-in rt [:patches]))))

(defn unwrap-pointers [pointers]
  (reduce-kv
   (fn [all k {:keys [target-event]}]
     (assoc all k target-event))
   {}
   pointers))

(defn update-match-buffer [rt segment]
  (s/assert :cep/runtime-iteration rt)
  (reduce-kv
   (fn [rt* state-name requested-pointers]
     (let [pattern-name (get-in rt* [:nfa :states state-name :state/pattern :pattern/name])
           pointers (unwrap-pointers requested-pointers)
           event (m/event segment state-name pattern-name pointers)]
       (doseq [{:keys [target-event n]} (vals requested-pointers)]
         (m/inc-refs! target-event state-name n))
       (-> rt*
           (update-in [:match-buffer state-name] m/append event)
           (assoc-in [:event-mapping state-name] event))))
   rt
   (get-in rt [:pointers])))

(defn update-last-event [rt]
  (s/assert :cep/runtime-iteration rt)
  (reduce
   (fn [rt* [patch k]]
     (let [from-name (get-in patch [:manifest/src-state :state/name])
           event (get (get-in rt [:event-mapping]) from-name)]
       (if (and event
                (not (some #{:ignore} #{(get-in patch [:manifest/transition :transition/kind])})))
         (assoc-in rt* [:patches k :manifest/dst-state :state/last-event] event)
         rt*)))
   rt
   (map vector (get-in rt [:patches]) (range))))

(defn apply-patches [rt]
  (s/assert :cep/runtime-iteration rt)
  (reduce
   (fn [rt* patch]
     (let [init-time (get-in patch [:manifest/src-state :state/init-time])
           kind (get-in patch [:manifest/transition :transition/kind])
           state (-> (:manifest/dst-state patch)
                     (assoc :state/version (:manifest/version patch))
                     (assoc :state/last-transition kind)
                     (assoc :state/init-time init-time)
                     (dissoc :state/transitions))
           reified-state (cond-> state
                           (:trace? rt)
                           (assoc :state/previous-id (get-in patch [:manifest/src-state :state/id]))

                           (:trace? rt)
                           (assoc :state/id (u/random-uuid))

                           (:trace? rt)
                           (with-meta (select-keys (meta (:manifest/src-state patch)) [:state/persistent-id])))]
       (update-in rt* [:pending-states] conj reified-state)))
   (assoc-in rt [:pending-states] [])
   (get-in rt [:patches])))

(defn trace-match [rt state last-event version]
  (cond-> (reverse (m/traverse-pointers last-event version))
    (:trace? rt)
    (with-meta (select-keys state [:state/previous-id]))))

(defn find-accelerated-matches [rt]
  (s/assert :cep/runtime-iteration rt)
  (reduce
   (fn [rt* state]
     (if (and (:state/accelerate? state)
              (not= (:state/name state) (get-in state [:state/last-event :state-name])))
       (let [{:state/keys [last-event version]} state
             match (trace-match rt* state last-event version)]
         (-> rt*
             (update :iteration-matches conj match)
             (update :match-buffer m/prune-match last-event version)
             (update :pending-states conj state)))
       (update rt* :pending-states conj state)))
   (assoc rt :pending-states [])
   (:pending-states rt)))

(defn find-final-matches [rt]
  (s/assert :cep/runtime-iteration rt)
  (reduce
   (fn [rt* state]
     (if (= (:state/name state) u/final-state-name)
       (let [{:state/keys [last-event version]} state
             match (trace-match rt* state last-event version)]
         (-> rt*
             (update :iteration-matches conj match)
             (update :match-buffer m/prune-match last-event version)))
       (update rt* :pending-states conj state)))
   (assoc rt :pending-states [])
   (:pending-states rt)))

(defn merge-matches [rt]
  (s/assert :cep/runtime-iteration rt)
  (update rt :matches into (distinct (:iteration-matches rt))))

(defn remove-transient-values [rt]
  (s/assert :cep/runtime-iteration rt)
  (-> rt
      (dissoc :processing-queue)
      (dissoc :patches)
      (dissoc :pointers)
      (dissoc :event-mapping)
      (dissoc :iteration-matches)))

(defn evaluate-event
  ([rt segment]
   (evaluate-event rt segment {}))
  ([rt segment {:keys [event-time id]}]
   (s/assert :cep/runtime rt)
   (let [result (-> rt
                    (initialize-transient-values)
                    (add-new-starting-state event-time id)
                    (prune-timed-out-runs event-time)
                    (load-processing-queue segment)
                    (evaluate-processing-queue segment)
                    (collapse-pointers)
                    (update-match-buffer segment)
                    (update-last-event)
                    (apply-patches)
                    (find-accelerated-matches)
                    (find-final-matches)
                    (merge-matches)
                    (remove-transient-values))]
     (s/assert :cep/runtime result)
     result)))
