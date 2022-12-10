(ns adventofcode-2022.exercises.day-10.part-2)
(require '[clojure.string :as str :refer [split]])

(defn print-signal
  [cycle x noop]
  (let [sprite #{(dec x) x (inc x)}
        vertical-cycle (rem cycle 40)
        prints (if noop [vertical-cycle] [vertical-cycle (rem (inc vertical-cycle) 40)])]
    (reduce
      (fn [signal s-cycle]
        (str signal (if (= s-cycle 0) "\n" "") (if (contains? sprite s-cycle) "#" ".")))
      ""
      prints)))

(defn run
  [inputs]
  (first (reduce
    (fn [[signal cycle x] [cmd v]]
      (let [noop (= cmd "noop")
            n-cycle (if noop (inc cycle) (+ 2 cycle))
            n-x (if noop x (+ x (Integer/parseInt v)))]
        [(str signal (print-signal cycle x noop)) n-cycle n-x]))
    ["" 0 1]
    (map #(split % #" ") inputs))))
