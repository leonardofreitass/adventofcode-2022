(ns adventofcode-2022.exercises.day-4.part-1)
(require '[clojure.string :refer [split]])

(defn overlaps?
  [xa xb ya yb]
  (or
    (<= xa ya yb xb)
    (<= ya xa xb yb)))

(defn run
  [inputs]
  (reduce
    (fn [acc line]
      (if (apply overlaps? (map #(Integer/parseInt %) (split line #"[-,]")))
        (inc acc)
        acc))
    0
    inputs))
