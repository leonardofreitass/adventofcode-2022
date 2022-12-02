(ns adventofcode-2022.exercises.day-2.part-2)

(require '[clojure.string :as str])

(def guide {
  "X" {:points 0 :pairs {"A" "C" "B" "A" "C" "B"}},
  "Y" {:points 3 :pairs {"A" "A" "B" "B" "C" "C"}},
  "Z" {:points 6 :pairs {"A" "B" "B" "C" "C" "A"}}
  "A" {:points 1}
  "B" {:points 2}
  "C" {:points 3}
})

(defn run
  [inputs]
  (reduce
    (fn [points play]
      (let [[l r] (str/split play #" ")
            strategy (guide r)
            play-points (:points strategy)
            pair ((:pairs strategy) l)
            pair-points (:points (guide pair))]
        (+ points play-points pair-points)))
    0
    inputs))
