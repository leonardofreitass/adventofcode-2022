(ns adventofcode-2022.exercises.day-5.part-1)
(require '[clojure.string :as str :refer [includes? blank?]])

(defn parse-stacks
  [initial-stacks]
  (vec (map
    (fn [v] (filter #(not= (str %) " ") v))
    (apply mapv vector (map 
      (fn [line]
        (map #(nth % 1) (partition 4 4 [" "] line))) 
      initial-stacks)))))

(defn run
  [inputs]
  (let [initial-stacks (filter #(includes? % "[") inputs)
        cmds (drop (+ 2 (count initial-stacks)) inputs)]
     (apply str (map
      #(first %)
      (reduce
        (fn [stacks cmd]
          (let [[n _from _to] (map #(Integer/parseInt %) (re-seq #"\d+" cmd))
                from (dec _from)
                to (dec _to)]
            (assoc stacks
              from (drop n (nth stacks from))
              to (apply conj (nth stacks to) (take n (nth stacks from))))))
        (parse-stacks initial-stacks)
        cmds)))))

