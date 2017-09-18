(ns metamorphic.examples
  (:refer-clojure :exclude [next])
  (:require [clojure.test :as t]
            [clojure.spec.alpha :as s]
            [metamorphic.api :refer :all]
            [metamorphic.runtime :as rt]
            [metamorphic.viz :as v])
  (:import [java.util.concurrent TimeUnit]))

(s/check-asserts true)

(defn above-average? [event history pattern-sequence pattern]
  (> (:temperature event) 80))

(defn warning? [event history pattern-sequence pattern]
  (> (:temperature event) 90))

(defn dangerous? [event history pattern-sequence pattern]
  (> (:temperature event) 100))

(t/deftest strictly-rising-temperatures
  (let [temperature-pattern
        {:pattern-sequence/name "rising temperatures"
         :pattern-sequence/patterns
         [{:pattern/name "above average"
           :pattern/predicate ::above-average?
           :pattern/contiguity :next
           :pattern/kind :singleton}

          {:pattern/name "warning"
           :pattern/predicate ::warning?
           :pattern/contiguity :next
           :pattern/kind :singleton}

          {:pattern/name "danger"
           :pattern/predicate ::dangerous?
           :pattern/contiguity :next
           :pattern/kind :singleton}]}
        runtime (rt/initialize-runtime temperature-pattern)
        events [{:temperature 60}
                {:temperature 82}
                {:temperature 78}
                {:temperature 85}
                {:temperature 93}
                {:temperature 102}
                {:temperature 101}
                {:temperature 84}
                {:temperature 68}]
        {:keys [matches]} (reduce rt/evaluate-event runtime events)]
    (t/is (= [[{:temperature 85} {:temperature 93} {:temperature 102}]
              [{:temperature 93} {:temperature 102} {:temperature 101}]]
             matches))))

(t/deftest progressively-rising-temperatures
  (let [temperature-pattern
        {:pattern-sequence/name "rising temperatures"
         :pattern-sequence/patterns
         [{:pattern/name "above average"
           :pattern/predicate ::above-average?
           :pattern/contiguity :next
           :pattern/kind :singleton}

          {:pattern/name "warning"
           :pattern/predicate ::warning?
           :pattern/contiguity :followed-by
           :pattern/kind :singleton}

          {:pattern/name "danger"
           :pattern/predicate ::dangerous?
           :pattern/contiguity :followed-by
           :pattern/kind :singleton}]}
        runtime (rt/initialize-runtime temperature-pattern)
        events [{:temperature 60}
                {:temperature 82}
                {:temperature 78}
                {:temperature 85}
                {:temperature 93}
                {:temperature 102}
                {:temperature 101}
                {:temperature 84}
                {:temperature 68}]
        {:keys [matches]} (reduce rt/evaluate-event runtime events)]
    (t/is (= [[{:temperature 82} {:temperature 93} {:temperature 102}]
              [{:temperature 85} {:temperature 93} {:temperature 102}]
              [{:temperature 93} {:temperature 102} {:temperature 101}]]
             matches))))

(t/deftest progressively-rising-temperatures-windowed
  (let [temperature-pattern
        {:pattern-sequence/name "rising temperatures"
         :pattern-sequence/within (.toMillis (TimeUnit/MINUTES) 1)
         :pattern-sequence/patterns
         [{:pattern/name "above average"
           :pattern/predicate ::above-average?
           :pattern/contiguity :next
           :pattern/kind :singleton}

          {:pattern/name "warning"
           :pattern/predicate ::warning?
           :pattern/contiguity :followed-by
           :pattern/kind :singleton}

          {:pattern/name "danger"
           :pattern/predicate ::dangerous?
           :pattern/contiguity :followed-by
           :pattern/kind :singleton}]}
        runtime (rt/initialize-runtime temperature-pattern)
        events [{:t 0 :temperature 85}
                {:t 50000 :temperature 95}
                {:t 70000 :temperature 110}

                {:t 100000 :temperature 83}
                {:t 105000 :temperature 75}
                {:t 130000 :temperature 92}
                {:t 140000 :temperature 85}
                {:t 150000 :temperature 101}]
        {:keys [matches timed-out-matches]}
        (reduce
         (fn [rt* event]
           (rt/evaluate-event rt* event {:event-time (:t event)}))
         runtime
         events)]
    (t/is (= [[{:t 100000 :temperature 83}
               {:t 130000 :temperature 92}
               {:t 150000 :temperature 101}]]
             matches))
    (t/is (= [[{:t 0 :temperature 85} {:t 50000 :temperature 95}]
              [{:t 50000 :temperature 95} {:t 70000 :temperature 110}]
              [{:t 70000 :temperature 110} {:t 130000 :temperature 92}]]
             timed-out-matches))))
