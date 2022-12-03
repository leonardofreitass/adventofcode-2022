(ns adventofcode-2022.exercises.day-3.part-2)
(require '[clojure.set :refer [intersection]])

(defn char->number
  [char]
  (let [char-n (int char)]
    (if (>= char-n 97)
      (- char-n 96)
      (- char-n 38))))

(defn run
  [inputs]
  (reduce
    (fn [acc bps]
      (+ acc
        (char->number
          (first
            (apply 
              intersection
              (map set bps))))))
    0
    (partition 3 inputs)))
