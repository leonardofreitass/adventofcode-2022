(ns adventofcode-2022.exercises.day-15.part-1)
(defn parse-int [s] (Integer/parseInt s))

(defn distance
  [[xa ya] [xb yb]]
  (+ (abs (- xa xb)) (abs (- ya yb))))

(defn target-line
  [[xa ya] [xb yb] target-y]
  (let [r (distance [xa ya] [xb yb])
        d (abs (- target-y ya))
        s (- r d)]
    (cond
      (< r d) nil
      (= r d) [xa xa]
      :else [(- xa s) (+ xa s)])))

(defn parse-input
  [inputs]
  (reduce
   (fn [acc line]
     (conj
      acc
      (map
       (fn [[_ & pos]] (vec (map parse-int pos)))
       (re-seq #"x=(-?\d+), y=(-?\d+)" line))))
   []
   inputs))

(defn map-coverage
  [sensor-beacon-info target-y]
  (reduce
   (fn [coverage [sensor beacon]]
     (let [cov-range (target-line sensor beacon target-y)]
       (if cov-range (conj coverage cov-range) coverage)))
   ()
   sensor-beacon-info))

(defn merge-coverage
  [coverage]
  (reduce
   (fn [cov [start end]]
     (let [[xa xb] (peek cov)]
       (if (>= xb start)
         (conj (pop cov) [xa (max end xb)])
         (conj cov [start end]))))
   [(first coverage)]
   (next coverage)))

(defn run
  [inputs]
  (reduce
   (fn [acc [start end]] (+ acc (- end start)))
   0
   (merge-coverage (sort (map-coverage (parse-input inputs) 2000000)))))

