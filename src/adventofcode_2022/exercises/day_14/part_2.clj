(ns adventofcode-2022.exercises.day-14.part-2)
(require '[clojure.string :refer [split]])
(require '[clojure.set :refer [difference]])
(require '[flatland.ordered.set :refer [ordered-set]])
(defn parse-int [s] (Integer/parseInt s))

(def hole [500 0])

(defn parse-inputs
  [inputs]
  (map
   (fn [line]
     (map #(map parse-int (split % #",")) (split line #" -> ")))
   inputs))

(defn make-line
  [[xa ya] [xb yb]]
  (for [x (range (min xa xb) (inc (max xa xb)))
        y (range (min ya yb) (inc (max ya yb)))]
    [x y]))

(defn make-wall
  [line]
  (loop [coordinates line
         pos #{}]
    (if (= 1 (count coordinates))
      pos
      (recur
       (next coordinates)
       (apply conj pos (apply make-line (take 2 coordinates)))))))

(defn draw-chart
  [inputs]
  (reduce
   (fn [[chart max-depth] line]
     (let [wall (make-wall line)]
       [(apply conj chart wall) (max max-depth (second (first (sort-by second > wall))))]))
   [#{} 0]
   (parse-inputs inputs)))

(defn drop-until-rest
  [chart max-depth]
  (loop [[x y] hole]
    (let [below-pos (ordered-set [x (inc y)] [(dec x) (inc y)] [(inc x) (inc y)])
          drop-to (difference below-pos (conj chart [x max-depth] [(dec x) max-depth] [(inc x) max-depth]))]
      (if (empty? drop-to)
        [x y]
        (recur (first drop-to))))))

(defn run
  [inputs]
  (let [[chart' max-depth] (draw-chart inputs)]
    (loop [chart chart'
           drops 0]
      (let [next-drop (drop-until-rest chart (+ 2 max-depth))]
        (if (= hole next-drop)
          (inc drops)
          (recur (conj chart next-drop) (inc drops)))))))
