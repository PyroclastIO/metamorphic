(ns metamorphic.validation
  (:require [clojure.string :as s]
            [metamorphic.util :as u]))

(defn validate-pattern-names-unique! [patterns]
  (let [pattern-names (map :pattern/name patterns)
        freqs (frequencies pattern-names)
        dupes (u/select-keys-by freqs (fn [k] (> k 1)))]
    (when (seq dupes)
      (throw (ex-info (str "Pattern names must be unique. The following were used more than once: " (s/join ", " (keys dupes)) ".")
                      dupes)))))

(defn validate-final-name! [patterns]
  (let [pattern-names (map :pattern/name patterns)
        final? (some #{u/final-state-name} pattern-names)]
    (when final?
      (throw (ex-info (str "Cannot use " u/final-state-name " as a pattern name. It is reserved.") {})))))

(defn validate-pattern-sequence! [pattern-sequence]
  (validate-pattern-names-unique! (:pattern-sequence/patterns pattern-sequence))
  (validate-final-name! (:pattern-sequence/patterns pattern-sequence)))
