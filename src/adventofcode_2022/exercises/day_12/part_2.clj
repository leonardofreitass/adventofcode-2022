(ns adventofcode-2022.exercises.day-12.part-2)
(require '[clojure.string :refer [index-of]])

(def start-value (int \a))
(def end-value (int \z))

(defn neighbors
  ([g n] (get g n {}))
  ([g n uv] (select-keys (neighbors g n) uv)))

(defn update-costs
  [g costs curr unvisited]
  (let [curr-cost (costs curr)]
    (reduce
     (fn [c [nbr nbr-cost]] (update-in c [nbr] (partial min (+ curr-cost nbr-cost))))
     costs
     (neighbors g curr unvisited))))

(defn crawl
  [g src]
  (loop [costs (assoc (zipmap (keys g) (repeat ##Inf)) src 0)
         curr src
         unvisited (disj (apply hash-set (keys g)) src)]
    (if (or (empty? unvisited) (= ##Inf (costs curr)))
      costs
      (let [costs' (update-costs g costs curr unvisited)
            curr' (first (sort-by costs' unvisited))]
        (recur costs'
               curr'
               (disj unvisited curr'))))))

(defn cross-pos
  [[x y]]
  [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])

(defn filter-valid-pos
  [heightmap pos]
  (fn [n-pos]
    (let [n-val (get-in heightmap n-pos)
          val (get-in heightmap pos)]
      (condp = n-val
        nil false
        (<= val (inc n-val))))))

(defn find-pos
  [inputs val]
  (loop [x 0
         rest inputs]
    (let [pos (index-of (first rest) val)]
      (if (not (nil? pos))
        [x pos]
        (recur (inc x) (next rest))))))

(defn create-graph
  [heightmap]
  (reduce
   (fn [graph pos]
     (assoc graph
            pos
            (zipmap
             (filter (filter-valid-pos heightmap pos) (cross-pos pos))
             (repeat 1))))
   {}
   (for [x (range (count heightmap))
         y (range (count (first heightmap)))]
     [x y])))

(defn run
  [inputs]
  (let [hm (vec (map #(vec (map int %)) inputs))
        end-pos (find-pos inputs "E")
        heightmap (assoc-in hm end-pos end-value)]
    (first (sort-by second (filter
                  (fn [[pos]]
                    (= start-value (get-in heightmap pos)))
                  (crawl (create-graph heightmap) end-pos))))))
