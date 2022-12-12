(ns adventofcode-2022.exercises.day-12.part-1)
(require '[clojure.set :refer [difference select]])

(def state (atom {}))
(def heightmap (atom []))
(def start-pos [21 0])
(def start-value 97)
(def end 69)
(def end-value 122)

(defn cross-pos
  [[x y]]
  #{[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]})

(defn filter-valid-pos
  [pos]
  (fn [n-pos]
    (let [n-val (get-in @heightmap n-pos)
          val (get-in @heightmap pos)]
      (condp = n-val
        nil false
        end (>= val (dec end-value))
        (>= val (dec n-val))))))

(defn crawl
  [visited pos]
  (if (contains? @state pos)
    (@state pos)
    (let [cross (difference (cross-pos pos) visited)
          valid-cross (select (filter-valid-pos pos) cross)
          ret (if (empty? valid-cross)
                ##Inf
                (apply min (map #(if (= end (get-in @heightmap %)) 1 (inc (trampoline crawl (conj visited pos) %))) valid-cross)))]
      (swap! state assoc pos ret)
      ret)))

(defn run
  [inputs]
  (let [hm (vec (map #(vec (map int %)) inputs))]
    (reset! heightmap (assoc-in hm start-pos start-value))
    (crawl #{} start-pos)))
