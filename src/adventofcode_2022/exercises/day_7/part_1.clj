(ns adventofcode-2022.exercises.day-7.part-1)
(require '[clojure.string :as str :refer [starts-with? split]])

(defn walk
  [path cd]
  (reduce
    (fn [new-path cmd]
      (let [[_ _ target] (split cmd #" ")]
        (if (= target "/")
          []
          (if (= target "..")
            (pop new-path)
            (conj new-path target)))))
    path
    cd))

(defn sum-size
  [index path size]
  (loop [next-path path
         new-index index]
    (if (empty? next-path)
      (update new-index "/" + size)
      (recur
        (pop next-path)
        (update new-index (str/join "-" next-path) + size)))))

(defn do-index
  [index path ls]
  (reduce
    (fn [new-index result]
      (let [[size dir] (split result #" ")
            target (conj path dir)]
        (if (= size "dir")
          (assoc new-index (str/join "-" target) 0)
          (sum-size new-index path (Integer/parseInt size)))))
    index
    ls))

(defn walk-and-index
  [inputs]
  (second (reduce
    (fn [[path index] [cd ls]]
      (let [new-path (walk path cd)
            new-index (do-index index new-path (next ls))]
        [new-path new-index]))
    [[] {"/" 0}]
    (partition 2 (partition-by #(starts-with? % "$ cd") inputs)))))

(defn run
  [inputs]
  (reduce-kv
    (fn [acc _ size]
      (if (<= size 100000)
        (+ size acc)
        acc))
    0
    (walk-and-index inputs)))
