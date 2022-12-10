(ns adventofcode-2022.exercises.day-10.part-1)
(require '[clojure.string :as str :refer [split]])

(defn run
  [inputs]
  (first (reduce
    (fn [[signal cycle x checkpoints] [cmd v]]
      (let [checkpoint (first checkpoints)
            noop (= cmd "noop")
            n-cycle (if noop (inc cycle) (+ 2 cycle))
            past-checkpoint (>= n-cycle checkpoint)
            n-signal (if past-checkpoint (+ signal (* checkpoint x)) signal)
            n-checkpoints (if past-checkpoint (next checkpoints) checkpoints)
            n-x (if noop x (+ x (Integer/parseInt v)))]
        [n-signal n-cycle n-x n-checkpoints]))
    [0 0 1 (iterate #(+ 40 %) 20)]
    (map #(split % #" ") inputs))))
