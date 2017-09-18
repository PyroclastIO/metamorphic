(ns metamorphic.spec
  (:require [clojure.spec.alpha :as s]))

(s/def :cep/pattern-sequence
  (s/keys :req [:pattern-sequence/name
                :pattern-sequence/patterns]
          :opt [:pattern-sequence/within]))

(s/def :pattern-sequence/name string?)

(s/def :pattern-sequence/within (s/nilable nat-int?))

(s/def :pattern-sequence/patterns
  (s/coll-of :pattern-sequence/pattern :kind vector? :min 1))

(defmulti pattern-kind :pattern/kind)

(defmethod pattern-kind :singleton
  [_]
  (s/keys :req [:pattern/name
                :pattern/kind
                :pattern/predicate
                :pattern/contiguity]
          :opt [:pattern/optional?]))

(defmethod pattern-kind :looping
  [_]
  (s/keys :req [:pattern/name
                :pattern/kind
                :pattern/predicate
                :pattern/contiguity
                :pattern/repetition]
          :opt [:pattern/optional?
                :pattern/looping-predicate
                :pattern/looping-contiguity]))

(s/def :pattern-sequence/pattern
  (s/multi-spec pattern-kind :pattern/kind))

(s/def :pattern/name string?)

(s/def :pattern/kind #{:singleton :looping})

(s/def :pattern/optional? boolean?)

(s/def :pattern/predicate (s/or :kw keyword? :f fn?))

(s/def :pattern/looping-predicate (s/or :kw keyword? :f fn?))

(s/def :pattern/contiguity #{:next :followed-by :followed-by-any})

(s/def :pattern/looping-contiguity :pattern/contiguity)

(s/def :pattern/repetition #{:one-or-more})

(s/def :state/name string?)

(s/def :state/version
  (s/coll-of nat-int? :kind vector? :min-count 1))

(s/def :state/kind #{:singleton :looping :final})

(s/def :state/pattern :pattern-sequence/pattern)

(s/def :state/contiguity :pattern/contiguity)

(s/def :state/accelerate? boolean?)

(s/def :state/id uuid?)

(s/def :state/previous-id :state/id)

(s/def :transition/to :state/name)

(s/def :transition/kind #{:begin :remain :proceed :ignore})

(s/def :transition/condition (s/or :fn fn? :var var?))

(s/def :state/transition
  (s/keys :req [:transition/to
                :transition/kind]
          :opt [:transition/condition]))

(s/def :state/transitions
  (s/map-of :transition/kind (s/coll-of :state/transition :kind vector?)))

(s/def :state/last-transition :transition/kind)

(s/def :cep-compiler/state
  (s/keys :req [:state/name
                :state/kind]
          :opt [:state/pattern
                :state/accelerate?
                :state/contiguity 
                :state/transitions]))

(s/def :cep-compiler/initial-state :state/name)

(s/def :cep-compiler/current-state :state/name)

(s/def :cep-compiler/match-period (s/nilable nat-int?))

(s/def :cep-compiler/states
  (s/map-of :state/name :cep-compiler/state))

(s/def :cep/nfa
  (s/keys :req-un [:cep-compiler/states
                   :cep-compiler/initial-state
                   :cep-compiler/current-state
                   :cep-compiler/match-period]))

(s/def :cep/next-root-version pos-int?)

(s/def :cep/match-buffer
  (s/map-of :state/name record?))

(s/def :state/init-time (s/nilable nat-int?))

(s/def :state/n-outgoing-begins nat-int?)

(s/def :state/n-outgoing-remains nat-int?)

(s/def :state/n-outgoing-proceeds nat-int?)

(s/def :state/n-outgoing-ignores nat-int?)

(s/def :cep/pending-state
  (s/keys :req [:state/name
                :state/kind]
          :opt [:state/init-time
                :state/version
                :state/last-transition
                :state/pattern
                :state/contiguity
                :state/accelerate?
                :state/id
                :state/previous-id
                :state/n-outgoing-begins
                :state/n-outgoing-remains
                :state/n-outgoing-proceeds
                :state/n-outgoing-ignores]))

(s/def :cep/pending-states
  (s/coll-of :cep/pending-state :kind sequential?))

(s/def :cep/matches
  (s/coll-of sequential?))

(s/def :cep/timed-out-matches
  (s/coll-of sequential?))

(s/def :manifest/src-state :cep/pending-state)

(s/def :manifest/dst-state :cep/pending-state)

(s/def :manifest/transition :state/transition)

(s/def :manifest/version :state/version)

(s/def :manifest/branching? boolean?)

(s/def :cep/transition-manifest
  (s/keys :req [:manifest/src-state
                :manifest/transition
                :manifest/branching?]))

(s/def :cep/patch
  (s/keys :req [:manifest/src-state
                :manifest/dst-state
                :manifest/version
                :manifest/transition
                :manifest/branching?]))

(s/def :cep/patches
  (s/coll-of :cep/patch :kind sequential?))

(s/def :cep/processing-queue
  (s/coll-of :cep/transition-manifest :kind sequential?))

(s/def :cep/runtime
  (s/keys :req-un [:cep/pattern-sequence
                   :cep/nfa
                   :cep/match-buffer
                   :cep/next-root-version
                   :cep/pending-states
                   :cep/matches
                   :cep/timed-out-matches]))

(s/def :cep/runtime-iteration
  (s/keys :req-un [:cep/pattern-sequence
                   :cep/nfa
                   :cep/match-buffer
                   :cep/next-root-version
                   :cep/pending-states
                   :cep/patches
                   :cep/matches
                   :cep/timed-out-matches
                   :cep/processing-queue]))
