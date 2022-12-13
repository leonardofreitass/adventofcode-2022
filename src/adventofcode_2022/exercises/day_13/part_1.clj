(ns adventofcode-2022.exercises.day-13.part-1)
(require '[clojure.string :as str :refer [replace]])

(defn parse-vec
  [str]
  (load-string (replace str #"," " ")))

(defn n-vec
  [n-or-vec]
  (if (number? n-or-vec) [n-or-vec] n-or-vec))

(defn compare-packets
  [left' right']
  (loop [left left'
         right right']
    (if (empty? right)
      (if (empty? left) 0 1)
      (if (empty? left)
        -1
        (let [a (first left)
              b (first right)
              any-vec (or (vector? a) (vector? b))
              comp (if any-vec (compare-packets (n-vec a) (n-vec b)) (compare a b))]
          (if (not (zero? comp))
            comp
            (recur (next left) (next right))))))))

(defn run
  [inputs]
  (first
   (reduce
    (fn [[acc index] [left right]]
      (if (= -1 (compare-packets left right))
        [(+ acc index) (inc index)]
        [acc (inc index)]))
    [0 1]
    (partition 2 (map parse-vec (filter seq inputs))))))
