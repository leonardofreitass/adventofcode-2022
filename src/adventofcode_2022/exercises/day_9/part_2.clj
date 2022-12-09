(ns adventofcode-2022.exercises.day-9.part-2)
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
    (and 
      (= -2 (- x h-x))
      (= -2 (- y h-y)))
    [(dec h-x) (dec h-y)]
    (and 
      (= 2 (- x h-x))
      (= -2 (- y h-y)))
    [(inc h-x) (dec h-y)]
    (and 
      (= -2 (- x h-x))
      (= 2 (- y h-y)))
    [(dec h-x) (inc h-y)]
    (and 
      (= 2 (- x h-x))
      (= 2 (- y h-y)))
    [(inc h-x) (inc h-y)]
    (= -2 (- x h-x)) [(dec h-x) h-y]
    (= 2 (- x h-x)) [(inc h-x) h-y]
    (= -2 (- y h-y)) [h-x (dec h-y)]
    (= 2 (- y h-y)) [h-x (inc h-y)]
    :else [x y]))

(defn move-rope
  [rope]
  (loop [rest rope
         new-rope (list (peek rope))]
    (if (= 1 (count rest))
      (vec new-rope)
      (let [tail (apply move-tail (take-last 2 rest))]
        (recur
          (conj (pop (pop rest)) tail)
          (conj new-rope tail))))))

(defn move
  [acc dir n]
  (reduce
    (fn [[map rope] _]
      (let [new-head (move-head (peek rope) dir)
            new-rope (move-rope (conj (pop rope) new-head))]
        [(conj map (join "-" (first new-rope))) new-rope]))
    acc
    (repeat n nil)))

(def rope-size 10)

(defn run
  [inputs]
  (count (first
    (reduce
      (fn [acc [dir n]]
        (move acc dir (Integer/parseInt n)))
      [#{} (vec (repeat rope-size [0 0]))]
      (map #(split % #" ") inputs)))))
