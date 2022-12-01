(ns adventofcode-2022.exercises.day-1.part-2)

(defn run
  [inputs]
  (reduce + (nth 
    (reduce
      (fn [[acc total] cal]
        (if (= cal "")
          [0 (vec (take 3 (sort > (conj total acc))))]
          [(+ acc (Integer/parseInt cal)) total]))
      [0 [0 0 0]]
      inputs)
    1)))
