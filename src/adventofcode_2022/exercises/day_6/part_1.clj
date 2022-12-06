(ns adventofcode-2022.exercises.day-6.part-1)

(defn queue
  ([coll]
    (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))

(def window-size 4)

(defn run
  [inputs]
  (loop [n window-size
         window (queue (take window-size (seq (first inputs))))
         rest (drop window-size (seq (first inputs)))]
    (if (= (count (set window)) window-size)
      n
      (recur
        (inc n)
        (pop (conj window (first rest)))
        (next rest)))))
