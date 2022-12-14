(ns adventofcode-2022.exercises.day-14.visualize)
(require '[clojure.string :refer [split join]])
(require '[clojure.set :refer [difference union]])
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

(defn print-chart
  [chart' drops]
  (let [x-sorted-chart (sort-by first chart')
        y-sorted-chart (sort-by second chart')
        min-x (first (first x-sorted-chart))
        max-x (first (last x-sorted-chart))
        max-y (second (last y-sorted-chart))
        chart (apply conj chart' (for [x (range (- min-x 110) (+ 105 max-x))][x (+ 2 max-y)]))]
    (join "\n" (for [y (range 0 (+ 3 max-y))]
                      (reduce
                       (fn [acc pos] (str acc (cond (contains? chart pos) "#" (contains? drops pos) "o" :else " ")))
                       ""
                       (for [x (range (- min-x 110) (+ 105 max-x))]
                         [x y]))))))

(defn run
  [inputs]
  (let [[chart' max-depth] (draw-chart inputs)]
    (loop [chart chart'
           drops #{}]
      (let [next-drop (drop-until-rest (union chart drops) (+ 2 max-depth))]
        (if (zero? (rem (count drops) 100)) (println (print-chart chart drops) "\nCounter:" (count drops)))
        (if (= hole next-drop)
          (count drops)
          (recur chart (conj drops next-drop)))))))
