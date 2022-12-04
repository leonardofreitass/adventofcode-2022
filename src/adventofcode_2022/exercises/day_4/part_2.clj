(ns adventofcode-2022.exercises.day-4.part-2)
(require '[clojure.string :refer [split]])

(defn overlaps?
  [xa xb ya yb]
  (or
    (<= xa ya xb)
    (<= xa yb xb)
    (<= ya xa yb)
    (<= ya xb yb)))

(defn run
  [inputs]
  (reduce
    (fn [acc line]
      (if (apply overlaps? (map #(Integer/parseInt %) (split line #"[-,]")))
        (inc acc)
        acc))
    0
    inputs))
