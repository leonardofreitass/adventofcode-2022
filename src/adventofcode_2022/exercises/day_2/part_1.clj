(ns adventofcode-2022.exercises.day-2.part-1)

(require '[clojure.string :as str])

(def guide {
  "X" {:points 1 :wins "C" :draw "A"},
  "Y" {:points 2 :wins "A" :draw "B"},
  "Z" {:points 3 :wins "B" :draw "C"}
})

(defn run
  [inputs]
  (reduce
    (fn [points play]
      (let [[l r] (str/split play #" ")
            strategy (guide r)
            play-points (:points strategy)]
        (if (= l (:draw strategy))
          (+ points 3 play-points)
          (if (= l (:wins strategy))
            (+ points 6 play-points)
            (+ points 0 play-points)))))
    0
    inputs))
