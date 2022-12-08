(ns adventofcode-2022.exercises.day-8.part-2)
(require '[clojure.string :as str :refer [split]])

(defn parse-inputs
  [inputs]
  (vec (map 
    (fn [line] 
      (vec (map
        #(Integer/parseInt %) 
        (split line #"")))) 
    inputs)))

(defn map-all-pos
  [max-x max-y]
  (for [x (range max-x)
        y (range max-y)]
    [x y]))

(defn map-cross-pos
  [c-x c-y max-x max-y]
  (partition-by
    (fn [[x y]] [(= c-x x) (= c-y y)]) 
    (for [x (range max-x)
        y (range max-y)
        :when (or (= c-x x) (= c-y y))]
      [x y])))

(defn map-view-pos
  [c-x c-y max-x max-y]
  (let [[n e m w s] (map-cross-pos c-x c-y max-x max-y)]
    [(reverse n) (reverse e) w s]))

(defn count-view
  [chart n view-pos]
  (loop [next-pos view-pos
         trees 0]
    (if (empty? next-pos)
      trees
      (if (>= (get-in chart (first next-pos)) n)
        (inc trees)
        (recur (next next-pos) (inc trees))))))

(defn run
  [inputs]
  (let [chart (parse-inputs inputs)
        max-x (count chart)
        max-y (count (first chart))]
    (reduce
      (fn [acc [x y]]
        (let [n (get-in chart [x y])]
          (if (or (= x 0) (= x (dec max-x)) (= y 0) (= y (dec max-y)))
            acc
            (max acc
              (reduce
                (fn [acc view-pos]
                  (* acc (count-view chart n view-pos)))
                1
                (map-view-pos x y max-x max-y))))))
      0
      (map-all-pos max-x max-y))))
