(ns metamorphic.api-test
  (:refer-clojure :exclude [next])
  (:require [clojure.test :as t]
            [clojure.spec.alpha :as s]
            [metamorphic.api :refer :all]
            [metamorphic.runtime :as rt]
            [metamorphic.viz :as v]
            [metamorphic.trace :as trace]))

(s/check-asserts true)

(t/deftest single-pattern
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & context] (> (:x event) 5)))
                    (rt/initialize-runtime))
        events [{:x 3} {:x 5} {:x 6} {:x 4} {:x 8} {:x 30} {:x 2}]
        {:keys [matches match-buffer nfa]} (reduce rt/evaluate-event runtime events)]
    (t/is (= (frequencies [[{:x 6}]
                           [{:x 8}]
                           [{:x 30}]])
             (frequencies matches)))))

(t/deftest chain-two-patterns
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & context] (> (:x event) 5)))
                    (next "bigger" (fn [event & context] (> (:x event) 9)))
                    (rt/initialize-runtime))
        events [{:x 3} {:x 6} {:x 2} {:x 7} {:x 10} {:x 11} {:x 19} {:x 5}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is (= (frequencies [[{:x 7} {:x 10}]
                           [{:x 10} {:x 11}]
                           [{:x 11} {:x 19}]])
             (frequencies matches)))))

(t/deftest chain-pairs
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (constantly true))
                    (followed-by "next" (constantly true))
                    (rt/initialize-runtime))
        events [{:x "a"} {:x "b"} {:x "c"} {:x "d"} {:x "e"}]
        {:keys [matches match-buffer nfa]} (reduce rt/evaluate-event runtime events)]
    (t/is (= (frequencies [[{:x "a"} {:x "b"}]
                           [{:x "b"} {:x "c"}]
                           [{:x "c"} {:x "d"}]
                           [{:x "d"} {:x "e"}]])
             (frequencies matches)))))

(t/deftest followed-by-looping
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (constantly true))
                    (followed-by "end" (constantly true) {:repetition :one-or-more})
                    (rt/initialize-runtime))
        events [{:x "a"} {:x "b"} {:x "c"} {:x "d"} {:x "e"}]
        {:keys [matches match-buffer nfa]} (reduce rt/evaluate-event runtime events)]
    (t/is (= (frequencies [[{:x "a"} {:x "b"}]
                           [{:x "a"} {:x "b"} {:x "c"}]
                           [{:x "b"} {:x "c"}]
                           [{:x "b"} {:x "c"} {:x "d"}]
                           [{:x "a"} {:x "b"} {:x "c"} {:x "d"}]
                           [{:x "c"} {:x "d"}]
                           [{:x "c"} {:x "d"} {:x "e"}]
                           [{:x "a"} {:x "b"} {:x "c"} {:x "d"} {:x "e"}]
                           [{:x "b"} {:x "c"} {:x "d"} {:x "e"}]
                           [{:x "d"} {:x "e"}]])
             (frequencies matches)))))

(t/deftest followed-by-any-contiguity
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (constantly true))
                    (followed-by-any "end" (constantly true))
                    (rt/initialize-runtime))
        events [{:x "a"} {:x "b"} {:x "c"} {:x "d"} {:x "e"}]
        {:keys [matches nfa match-buffer nfa]} (reduce rt/evaluate-event runtime events)]
    (t/is (= (frequencies [[{:x "a"} {:x "b"}]
                           [{:x "a"} {:x "c"}]
                           [{:x "a"} {:x "d"}]
                           [{:x "a"} {:x "e"}]
                           [{:x "b"} {:x "c"}]
                           [{:x "b"} {:x "d"}]
                           [{:x "b"} {:x "e"}]
                           [{:x "c"} {:x "d"}]
                           [{:x "c"} {:x "e"}]
                           [{:x "d"} {:x "e"}]])
             (frequencies matches)))))

(t/deftest basic-pattern
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "start")))
                    (followed-by "middle" (fn [event & _] (= (:x event) "middle")))
                    (followed-by "end" (fn [event & _] (= (:x event) "end")))
                    (rt/initialize-runtime))
        events [{:x "start"} {:x "middle"} {:x "end"}]
        {:keys [matches match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is (= (frequencies [[{:x "start"} {:x "middle"} {:x "end"}]])
             (frequencies matches)))))

(t/deftest strict-contiguity
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "a")))
                    (next "middle" (fn [event & _] (= (:x event) "b")))
                    (rt/initialize-runtime))
        events [{:x "a"} {:x "b"}]
        {:keys [matches match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is (= (frequencies [[{:x "a"} {:x "b"}]])
             (frequencies matches)))))

(t/deftest strict-contiguity-miss
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "a")))
                    (next "middle" (fn [event & _] (= (:x event) "b")))
                    (rt/initialize-runtime))
        events [{:x "a"} {:x "c"} {:x "b"}]
        {:keys [matches]} (reduce rt/evaluate-event runtime events)]
    (t/is (= [] matches))))

(t/deftest branching-pattern
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "a")))
                    (followed-by-any "middle-1" (fn [event & _] (= (:x event) "b")))
                    (followed-by-any "middle-2" (fn [event & _] (= (:x event) "c")))
                    (followed-by-any "end" (fn [event & _] (= (:x event) "d")))
                    (rt/initialize-runtime))
        events [{:x "a"}
                {:x "b" :id 1}
                {:x "b" :id 2}
                {:x "b" :id 3}
                {:x "c" :id 1}
                {:x "c" :id 2}
                {:x "d"}]
        {:keys [matches nfa match-buffer pending-states]} (reduce rt/evaluate-event runtime events)]
    (t/is (= (frequencies
              [[{:x "a"} {:x "b" :id 1} {:x "c" :id 1} {:x "d"}]
               [{:x "a"} {:x "b" :id 2} {:x "c" :id 1} {:x "d"}]
               [{:x "a"} {:x "b" :id 3} {:x "c" :id 1} {:x "d"}]
               [{:x "a"} {:x "b" :id 1} {:x "c" :id 2} {:x "d"}]
               [{:x "a"} {:x "b" :id 2} {:x "c" :id 2} {:x "d"}]
               [{:x "a"} {:x "b" :id 3} {:x "c" :id 2} {:x "d"}]])
             (frequencies matches)))))

(t/deftest optional-branching-pattern
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "c")))
                    (followed-by-any "middle" (fn [event & _] (= (:x event) "a"))
                                     {:repetition :one-or-more
                                      :allow-combinations? true
                                      :optional? true})
                    (followed-by-any "end-1" (fn [event & _] (= (:x event) "b")))
                    (followed-by-any "end-2" (fn [event & _] (= (:x event) "d")))
                    (followed-by-any "end-3" (fn [event & _] (= (:x event) "e")))
                    (rt/initialize-runtime))
        events [{:x "c"}
                {:x "a" :id 1}
                {:x "a" :id 2}
                {:x "a" :id 3}
                {:x "b"}
                {:x "d" :id 1}
                {:x "d" :id 2}
                {:x "e"}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is
     (= (frequencies
         [[{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "a" :id 3} {:x "b"} {:x "d" :id 2} {:x "e"}]
          [{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "b"} {:x "d" :id 2} {:x "e"}]
          [{:x "c"} {:x "a" :id 1} {:x "a" :id 3} {:x "b"} {:x "d" :id 2} {:x "e"}]
          [{:x "c"} {:x "a" :id 2} {:x "a" :id 3} {:x "b"} {:x "d" :id 2} {:x "e"}]
          [{:x "c"} {:x "a" :id 1} {:x "b"} {:x "d" :id 2} {:x "e"}]
          [{:x "c"} {:x "a" :id 2} {:x "b"} {:x "d" :id 2} {:x "e"}]
          [{:x "c"} {:x "a" :id 3} {:x "b"} {:x "d" :id 2} {:x "e"}]
          [{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "a" :id 3} {:x "b"} {:x "d" :id 1} {:x "e"}]
          [{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "b"} {:x "d" :id 1} {:x "e"}]
          [{:x "c"} {:x "a" :id 1} {:x "a" :id 3} {:x "b"} {:x "d" :id 1} {:x "e"}]
          [{:x "c"} {:x "a" :id 2} {:x "a" :id 3} {:x "b"} {:x "d" :id 1} {:x "e"}]
          [{:x "c"} {:x "a" :id 1} {:x "b"} {:x "d" :id 1} {:x "e"}]
          [{:x "c"} {:x "a" :id 2} {:x "b"} {:x "d" :id 1} {:x "e"}]
          [{:x "c"} {:x "a" :id 3} {:x "b"} {:x "d" :id 1} {:x "e"}]
          [{:x "c"} {:x "b"} {:x "d" :id 1} {:x "e"}]
          [{:x "c"} {:x "b"} {:x "d" :id 2} {:x "e"}]])
        (frequencies matches)))))

(t/deftest zero-or-more
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "c")))
                    (followed-by-any "middle" (fn [event & _] (= (:x event) "a"))
                                     {:repetition :one-or-more
                                      :allow-combinations? true
                                      :optional? true})
                    (followed-by-any "end" (fn [event & _] (= (:x event) "b")))
                    (rt/initialize-runtime))
        events [{:x "c"}
                {:x "a" :id 1}
                {:x "a" :id 2}
                {:x "b"}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is
     (= (frequencies [[{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "b"}]
                      [{:x "c"} {:x "a" :id 1} {:x "b"}]
                      [{:x "c"} {:x "a" :id 2} {:x "b"}]
                      [{:x "c"} {:x "b"}]])
        (frequencies matches)))))

(t/deftest begin-zero-or-more
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "a"))
                           {:repetition :one-or-more
                            :optional? true})
                    (followed-by "end" (fn [event & _] (= (:x event) "b")))
                    (rt/initialize-runtime))
        events [{:x "a" :id 1}
                {:x "a" :id 2}
                {:x "a" :id 3}
                {:x "b"}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is
     (= (frequencies
         [[{:x "b"}]
          [{:x "a" :id 1} {:x "b"}]
          [{:x "a" :id 2} {:x "b"}]
          [{:x "a" :id 3} {:x "b"}]
          [{:x "a" :id 1} {:x "a" :id 2} {:x "b"}]
          [{:x "a" :id 2} {:x "a" :id 3} {:x "b"}]
          [{:x "a" :id 1} {:x "a" :id 2} {:x "a" :id 3} {:x "b"}]])
        (frequencies matches)))))

(t/deftest zero-or-more-repeated
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "c")))
                    (followed-by-any "middle-1" (fn [event & _] (= (:x event) "a"))
                                     {:repetition :one-or-more
                                      :allow-combinations? true
                                      :optional? true})
                    (followed-by "middle-2" (fn [event & _] (= (:x event) "d"))
                                 {:repetition :one-or-more
                                  :allow-combinations? true
                                  :optional? true})
                    (followed-by "end" (fn [event & _] (= (:x event) "e")))
                    (rt/initialize-runtime))
        events [{:x "c"}
                {:x "a"}
                {:x "d" :id 1}
                {:x "d" :id 2}
                {:x "e"}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is
     (= (frequencies
         [[{:x "c"} {:x "a"} {:x "d" :id 1} {:x "d" :id 2} {:x "e"}]
          [{:x "c"} {:x "a"} {:x "d" :id 1} {:x "e"}]
          [{:x "c"} {:x "d" :id 1} {:x "d" :id 2} {:x "e"}]
          [{:x "c"} {:x "d" :id 1} {:x "e"}]
          [{:x "c"} {:x "a"} {:x "e"}]
          [{:x "c"} {:x "e"}]])
        (frequencies matches)))))

(t/deftest zero-or-more-post-branching
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "c")))
                    (followed-by-any "branching" (fn [event & _] (= (:x event) "a")))
                    (followed-by-any "merging" (fn [event & _] (= (:x event) "f")))
                    (followed-by-any "looping" (fn [event & _] (= (:x event) "d"))
                                     {:repetition :one-or-more
                                      :allow-combinations? true
                                      :optional? true})
                    (followed-by "end" (fn [event & _] (= (:x event) "e")))
                    (rt/initialize-runtime))
        events [{:x "c"}
                {:x "a" :id 1}
                {:x "a" :id 2}
                {:x "f"}
                {:x "d" :id 1}
                {:x "d" :id 2}
                {:x "e"}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is
     (=
      (frequencies
       [[{:x "c"} {:x "a" :id 1} {:x "f"} {:x "e"}]
        [{:x "c"} {:x "a" :id 1} {:x "f"} {:x "d" :id 1} {:x "e"}]
        [{:x "c"} {:x "a" :id 1} {:x "f"} {:x "d" :id 2} {:x "e"}]
        [{:x "c"} {:x "a" :id 1} {:x "f"} {:x "d" :id 1} {:x "d" :id 2} {:x "e"}]
        [{:x "c"} {:x "a" :id 2} {:x "f"} {:x "e"}]
        [{:x "c"} {:x "a" :id 2} {:x "f"} {:x "d" :id 1} {:x "e"}]
        [{:x "c"} {:x "a" :id 2} {:x "f"} {:x "d" :id 2} {:x "e"}]
        [{:x "c"} {:x "a" :id 2} {:x "f"} {:x "d" :id 1} {:x "d" :id 2} {:x "e"}]])
      (frequencies matches)))))

(t/deftest next-with-optional-no-results
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "d")))
                    (followed-by "middle" (fn [event & _] (= (:x event) "a"))
                                 {:repetition :one-or-more
                                  :optional? true})
                    (next "end" (fn [event & _] (= (:x event) "b")))
                    (rt/initialize-runtime))
        events [{:x "d"}
                {:x "a" :id 1}
                {:x "a" :id 2}
                {:x "c"}
                {:x "b"}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is (= [] matches))))

(t/deftest eager-zero-or-more
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "c")))
                    (followed-by "middle" (fn [event & _] (= (:x event) "a"))
                                 {:repetition :one-or-more
                                  :optional? true})
                    (followed-by "end" (fn [event & _] (= (:x event) "b")))
                    (rt/initialize-runtime))
        events [{:x "c"}
                {:x "a" :id 1}
                {:x "a" :id 2}
                {:x "a" :id 3}
                {:x "b"}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is (= (frequencies
              [[{:x "c"} {:x "b"}]
               [{:x "c"} {:x "a" :id 1} {:x "b"}]
               [{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "b"}]
               [{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "a" :id 3} {:x "b"}]])
             (frequencies matches)))))

(t/deftest begin-at-least-one
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "a"))
                           {:repetition :one-or-more
                            :allow-combinations? true})
                    (followed-by "end" (fn [event & _] (= (:x event) "b")))
                    (rt/initialize-runtime))
        events [{:x "a" :id 1}
                {:x "a" :id 2}
                {:x "a" :id 3}
                {:x "b"}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is
     (= (frequencies
         [[{:x "a" :id 1} {:x "b"}]
          [{:x "a" :id 2} {:x "b"}]
          [{:x "a" :id 3} {:x "b"}]
          [{:x "a" :id 1} {:x "a" :id 2} {:x "b"}]
          [{:x "a" :id 1} {:x "a" :id 3} {:x "b"}]
          [{:x "a" :id 2} {:x "a" :id 3} {:x "b"}]
          [{:x "a" :id 1} {:x "a" :id 2} {:x "a" :id 3} {:x "b"}]])
        (frequencies matches)))))

(t/deftest next-zero-or-more
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "a")))
                    (next "middle" (fn [event & _] (= (:x event) "b"))
                          {:repetition :one-or-more
                           :consecutive? true
                           :optional? true})
                    (followed-by "end" (fn [event & _] (= (:x event) "c")))
                    (rt/initialize-runtime))
        events [{:x "a"}
                {:x "d"}
                {:x "b" :id 1}
                {:x "b" :id 2}
                {:x "b" :id 3}
                {:x "c"}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is (= (frequencies
              [[{:x "a"} {:x "c"}]])
             (frequencies matches)))))

(t/deftest at-least-one
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "c")))
                    (followed-by-any "middle" (fn [event & _] (= (:x event) "a"))
                                     {:repetition :one-or-more
                                      :allow-combinations? true})
                    (followed-by-any "end" (fn [event & _] (= (:x event) "b")))
                    (rt/initialize-runtime))
        events [{:x "c"}
                {:x "a" :id 1}
                {:x "a" :id 2}
                {:x "b"}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is
     (= (frequencies
         [[{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "b"}]
          [{:x "c"} {:x "a" :id 1} {:x "b"}]
          [{:x "c"} {:x "a" :id 2} {:x "b"}]])
        (frequencies matches)))))

(t/deftest at-least-one-eager
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "c")))
                    (followed-by-any "middle" (fn [event & _] (= (:x event) "a"))
                                     {:repetition :one-or-more})
                    (followed-by-any "end" (fn [event & _] (= (:x event) "b")))
                    (rt/initialize-runtime))
        events [{:x "c"}
                {:x "a" :id 1}
                {:x "a" :id 2}
                {:x "a" :id 3}
                {:x "b"}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is (= (frequencies
              [[{:x "c"} {:x "a" :id 1} {:x "b"}]
               [{:x "c"} {:x "a" :id 2} {:x "b"}]
               [{:x "c"} {:x "a" :id 3} {:x "b"}]
               [{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "b"}]
               [{:x "c"} {:x "a" :id 2} {:x "a" :id 3} {:x "b"}]
               [{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "a" :id 3} {:x "b"}]])
             (frequencies matches)))))

(t/deftest test-optional
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "c")))
                    (followed-by "middle" (fn [event & _] (= (:x event) "a"))
                                 {:optional? true})
                    (followed-by "end" (fn [event & _] (= (:x event) "b")))
                    (rt/initialize-runtime))
        events [{:x "c"}
                {:x "a"}
                {:x "b"}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is (= (frequencies
              [[{:x "c"} {:x "b"}]
               [{:x "c"} {:x "a"} {:x "b"}]])
             (frequencies matches)))))

(t/deftest start-and-end-with-zero-or-more
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "a"))
                           {:repetition :one-or-more
                            :optional? true})
                    (rt/initialize-runtime))
        events [{:x "c"}
                {:x "a" :id 1}
                {:x "a" :id 2}
                {:x "a" :id 3}
                {:x "d" :id 1}
                {:x "d" :id 2}
                {:x "d" :id 3}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is
     (= (frequencies
         [[{:x "a" :id 1}]
          [{:x "a" :id 2}]
          [{:x "a" :id 3}]
          [{:x "a" :id 1} {:x "a" :id 2}]
          [{:x "a" :id 2} {:x "a" :id 3}]
          [{:x "a" :id 1} {:x "a" :id 2} {:x "a" :id 3}]])
        (frequencies matches)))))

(t/deftest end-with-optional
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "c")))
                    (followed-by "middle" (fn [event & _] (= (:x event) "a"))
                                 {:optional? true})
                    (rt/initialize-runtime))
        events [{:x "c"}
                {:x "a"}]
        {:keys [matches nfa]} (reduce rt/evaluate-event runtime events)]
    (t/is
     (= (frequencies
         [[{:x "c"}]
          [{:x "c"} {:x "a"}]])
        (frequencies matches)))))

(t/deftest end-with-one-or-more
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "c")))
                    (followed-by "middle" (fn [event & _] (= (:x event) "a"))
                                 {:repetition :one-or-more})
                    (rt/initialize-runtime))
        events [{:x "c"}
                {:x "a" :id 1}
                {:x "a" :id 2}
                {:x "a" :id 3}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is (= (frequencies
              [[{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "a" :id 3}]
               [{:x "c"} {:x "a" :id 1} {:x "a" :id 2}]
               [{:x "c"} {:x "a" :id 1}]])
             (frequencies matches)))))

(t/deftest end-with-zero-or-more
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "c")))
                    (followed-by "middle" (fn [event & _] (= (:x event) "a"))
                                 {:repetition :one-or-more
                                  :optional? true})
                    (rt/initialize-runtime))
        events [{:x "c"}
                {:x "a" :id 1}
                {:x "a" :id 2}
                {:x "a" :id 3}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is
     (= (frequencies [[{:x "c"}]
                      [{:x "c"} {:x "a" :id 1}]
                      [{:x "c"} {:x "a" :id 1} {:x "a" :id 2}]
                      [{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "a" :id 3}]])
        (frequencies matches)))))

(t/deftest mixed-branching
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "a")))
                    (followed-by-any "middle-1" (fn [event & _] (= (:x event) "b")))
                    (followed-by "middle-2" (fn [event & _] (= (:x event) "c")))
                    (followed-by-any "end" (fn [event & _] (= (:x event) "d")))
                    (rt/initialize-runtime))
        events [{:x "a"}
                {:x "b" :id 1}
                {:x "b" :id 2}
                {:x "b" :id 3}
                {:x "c" :id 1}
                {:x "c" :id 2}
                {:x "d"}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is
     (= (frequencies [[{:x "a"} {:x "b" :id 1} {:x "c" :id 1} {:x "d"}]
                      [{:x "a"} {:x "b" :id 2} {:x "c" :id 1} {:x "d"}]
                      [{:x "a"} {:x "b" :id 3} {:x "c" :id 1} {:x "d"}]])
        (frequencies matches)))))

(t/deftest branched-skip-till-next
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "a")))
                    (followed-by "middle-1" (fn [event & _] (= (:x event) "b")))
                    (followed-by "middle-2" (fn [event & _] (= (:x event) "c")))
                    (followed-by-any "end" (fn [event & _] (= (:x event) "d")))
                    (rt/initialize-runtime))
        events [{:x "a"}
                {:x "b" :id 1}
                {:x "b" :id 2}
                {:x "b" :id 3}
                {:x "c" :id 1}
                {:x "c" :id 2}
                {:x "d"}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is
     (= (frequencies [[{:x "a"} {:x "b" :id 1} {:x "c" :id 1} {:x "d"}]])
        (frequencies matches)))))

(t/deftest strict-one-or-more
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "c")))
                    (followed-by "middle" (fn [event & _] (= (:x event) "a"))
                                 {:repetition :one-or-more
                                  :consecutive? true})
                    (followed-by "end" (fn [event & _] (= (:x event) "b")))
                    (rt/initialize-runtime))
        events [{:x "c"}
                {:x "d" :id 1}
                {:x "a" :id 1}
                {:x "a" :id 2}
                {:x "a" :id 3}
                {:x "d" :id 2}
                {:x "a" :id 4}
                {:x "b"}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is
     (= (frequencies [[{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "a" :id 3} {:x "b"}]
                      [{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "b"}]
                      [{:x "c"} {:x "a" :id 1} {:x "b"}]])
        (frequencies matches)))))

(t/deftest skip-till-next-one-or-more
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "c")))
                    (followed-by "middle" (fn [event & _] (= (:x event) "a"))
                                 {:repetition :one-or-more})
                    (followed-by "end" (fn [event & _] (= (:x event) "b")))
                    (rt/initialize-runtime))
        events [{:x "c"}
                {:x "d" :id 1}
                {:x "a" :id 1}
                {:x "a" :id 2}
                {:x "a" :id 3}
                {:x "d" :id 2}
                {:x "a" :id 4}
                {:x "b"}]
        {:keys [matches nfa match-buffer pending-states]} (reduce rt/evaluate-event runtime events)]
    (t/is
     (= (frequencies [[{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "a" :id 3} {:x "a" :id 4} {:x "b"}]
                      [{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "a" :id 3} {:x "b"}]
                      [{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "b"}]
                      [{:x "c"} {:x "a" :id 1} {:x "b"}]])
        (frequencies matches)))))

(t/deftest skip-till-any-one-or-more
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "c")))
                    (followed-by "middle" (fn [event & _] (= (:x event) "a"))
                                 {:repetition :one-or-more
                                  :allow-combinations? true})
                    (followed-by "end" (fn [event & _] (= (:x event) "b")))
                    (rt/initialize-runtime))
        events [{:x "c"}
                {:x "d" :id 1}
                {:x "a" :id 1}
                {:x "a" :id 2}
                {:x "a" :id 3}
                {:x "d" :id 2}
                {:x "a" :id 4}
                {:x "b"}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is
     (= (frequencies
         [[{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "a" :id 3} {:x "a" :id 4} {:x "b"}]
          [{:x "c"} {:x "a" :id 1} {:x "a" :id 3} {:x "a" :id 4} {:x "b"}]
          [{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "a" :id 3} {:x "b"}]
          [{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "a" :id 4} {:x "b"}]
          [{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "b"}]
          [{:x "c"} {:x "a" :id 1} {:x "a" :id 3} {:x "b"}]
          [{:x "c"} {:x "a" :id 1} {:x "a" :id 4} {:x "b"}]
          [{:x "c"} {:x "a" :id 1} {:x "b"}]])
        (frequencies matches)))))

(t/deftest strict-eager-zero-or-more
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "c")))
                    (followed-by "middle" (fn [event & _] (= (:x event) "a"))
                                 {:repetition :one-or-more
                                  :consecutive? true
                                  :optional? true})
                    (followed-by "end" (fn [event & _] (= (:x event) "b")))
                    (rt/initialize-runtime))
        events [{:x "c"}
                {:x "d" :id 1}
                {:x "a" :id 1}
                {:x "a" :id 2}
                {:x "a" :id 3}
                {:x "d" :id 2}
                {:x "a" :id 4}
                {:x "b"}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is
     (= (frequencies [[{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "a" :id 3} {:x "b"}]
                      [{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "b"}]
                      [{:x "c"} {:x "a" :id 1} {:x "b"}]
                      [{:x "c"} {:x "b"}]])
        (frequencies matches)))))

(t/deftest skip-till-any-zero-or-more
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "c")))
                    (followed-by "middle" (fn [event & _] (= (:x event) "a"))
                                 {:repetition :one-or-more
                                  :allow-combinations? true
                                  :optional? true})
                    (followed-by "end" (fn [event & _] (= (:x event) "b")))
                    (rt/initialize-runtime))
        events [{:x "c"}
                {:x "d" :id 1}
                {:x "a" :id 1}
                {:x "a" :id 2}
                {:x "a" :id 3}
                {:x "d" :id 2}
                {:x "a" :id 4}
                {:x "b"}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is
     (= (frequencies [[{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "a" :id 3} {:x "a" :id 4} {:x "b"}]
                      [{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "a" :id 4} {:x "b"}]
                      [{:x "c"} {:x "a" :id 1} {:x "a" :id 3} {:x "a" :id 4} {:x "b"}]
                      [{:x "c"} {:x "a" :id 1} {:x "a" :id 4} {:x "b"}]
                      [{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "a" :id 3} {:x "b"}] 
                      [{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "b"}]
                      [{:x "c"} {:x "a" :id 1} {:x "a" :id 3} {:x "b"}]
                      [{:x "c"} {:x "a" :id 1} {:x "b"}]
                      [{:x "c"} {:x "b"}]])
        (frequencies matches)))))

(t/deftest skip-till-next-zero-or-more
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "c")))
                    (followed-by "middle" (fn [event & _] (= (:x event) "a"))
                                 {:repetition :one-or-more
                                  :optional? true})
                    (followed-by "end" (fn [event & _] (= (:x event) "b")))
                    (rt/initialize-runtime))
        events [{:x "c"}
                {:x "d" :id 1}
                {:x "a" :id 1}
                {:x "a" :id 2}
                {:x "a" :id 3}
                {:x "d" :id 2}
                {:x "a" :id 4}
                {:x "b"}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is
     (= (frequencies [[{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "a" :id 3} {:x "a" :id 4} {:x "b"}]
                      [{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "a" :id 3} {:x "b"}]
                      [{:x "c"} {:x "a" :id 1} {:x "a" :id 2} {:x "b"}]
                      [{:x "c"} {:x "a" :id 1} {:x "b"}]
                      [{:x "c"} {:x "b"}]])
        (frequencies matches)))))

(t/deftest start-with-one-or-more-strict
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "a"))
                           {:repetition :one-or-more
                            :consecutive? true})
                    (rt/initialize-runtime))
        events [{:x "c" :id 1}
                {:x "a" :id 1}
                {:x "c" :id 2}
                {:x "a" :id 2}
                {:x "a" :id 3}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is
     (= (frequencies [[{:x "a" :id 1}]
                      [{:x "a" :id 2}]
                      [{:x "a" :id 3}]
                      [{:x "a" :id 2} {:x "a" :id 3}]])
        (frequencies matches)))))

(t/deftest start-with-zero-or-more-strict
  (let [runtime (-> (new-pattern-sequence "test")
                    (begin "start" (fn [event & _] (= (:x event) "a"))
                           {:repetition :one-or-more
                            :consecutive? true
                            :optional? true})
                    (rt/initialize-runtime))
        events [{:x "c" :id 1}
                {:x "a" :id 1}
                {:x "c" :id 2}
                {:x "a" :id 2}
                {:x "a" :id 3}]
        {:keys [matches nfa match-buffer]} (reduce rt/evaluate-event runtime events)]
    (t/is
     (= (frequencies [[{:x "a" :id 1}]
                      [{:x "a" :id 2}]
                      [{:x "a" :id 3}]
                      [{:x "a" :id 2} {:x "a" :id 3}]])
        (frequencies matches)))))
