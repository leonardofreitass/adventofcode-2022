(ns adventofcode-2022.exercises.day-13.part-2)
(require '[clojure.string :as str :refer [replace]])

(defn parse-vec
  [str]
  (load-string (replace str #"," " ")))

(def divider-packets
  #{"[[2]]" "[[6]]"})

(defn n-vec
  [n-or-vec]
  (if (number? n-or-vec) [n-or-vec] n-or-vec))

(defn compare-packets
  [left' right']
  (loop [left left'
         right right']
    (if (empty? right)
      (if (empty? left) 0 1)
      (if (empty? left)
        -1
        (let [a (first left)
              b (first right)
              any-vec (or (vector? a) (vector? b))
              comp (if any-vec (compare-packets (n-vec a) (n-vec b)) (compare a b))]
          (if (not (zero? comp))
            comp
            (recur (next left) (next right))))))))

(defn run
  [inputs]
  (reduce-kv
   (fn [acc i packet]
     (if (contains? divider-packets (str packet))
       (* acc (inc i))
       acc))
   1
   (vec (sort
    compare-packets
    (apply conj (map parse-vec (filter seq inputs))
           (map load-string divider-packets))))))

