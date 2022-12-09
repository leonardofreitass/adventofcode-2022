(ns adventofcode-2022.exercises.day-9.part-1)
(require '[clojure.string :as str :refer [split join]])

(defn move-head
  [[x y] dir]
  (case dir
    "R" [(inc x) y]
    "L" [(dec x) y]
    "U" [x (inc y)]
    "D" [x (dec y)]))

(defn move-tail
  [[x y] [h-x h-y]]
  (cond
    (= -2 (- x h-x)) [(dec h-x) h-y]
    (= 2 (- x h-x)) [(inc h-x) h-y]
    (= -2 (- y h-y)) [h-x (dec h-y)]
    (= 2 (- y h-y)) [h-x (inc h-y)]
    :else [x y]))

(defn move
  [acc dir n]
  (reduce
    (fn [[map head tail] _]
      (let [new-head (move-head head dir)
            new-tail (move-tail tail new-head)]
        [(conj map (join "-" new-tail)) new-head new-tail]))
    acc
    (repeat n nil)))

(defn run
  [inputs]
  (count (first
    (reduce
      (fn [acc [dir n]]
        (move acc dir (Integer/parseInt n)))
      [#{} [0 0] [0 0]]
      (map #(split % #" ") inputs)))))
