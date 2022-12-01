(ns adventofcode-2022.exercises.day-1.part-1)

(defn run
  [inputs]
  (nth 
    (reduce
      (fn [[acc total] cal]
        (if (= cal "")
          [0 (max acc total)]
          [(+ acc (Integer/parseInt cal)) total]))
      [0 0]
      inputs)
    1))         
