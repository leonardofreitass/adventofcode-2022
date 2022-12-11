(ns adventofcode-2022.exercises.day-11.part-1)

(defn item-or-n
  [item n]
  (if (= "old" n) item (Integer/parseInt n)))

(defn make-op-fn
  [[left op right]]
  (fn [item]
    (let [parsed-left (item-or-n item left)
          parsed-right (item-or-n item right)]
      (quot ((resolve (symbol op)) parsed-left parsed-right) 3))))

(defn make-next-monkey-fn
  [n iftrue iffalse]
  (fn [item] (if (zero? (mod item n)) iftrue iffalse)))

(defn parse-input
  [inputs]
  (reduce
   (fn [acc [monkey start operation test iftrue iffalse]]
     (let [id (re-find #"\d+" monkey)
           items (map #(Integer/parseInt %) (re-seq #"\d+" start))
           [_ & op] (re-find #"new = ([^\s]+) ([^\s]+) ([^\s]+)" operation)
           div (Integer/parseInt (re-find #"\d+" test))
           monkey-true (re-find #"\d+" iftrue)
           monkey-false (re-find #"\d+" iffalse)]
       (assoc acc id {:items items :op (make-op-fn op) :next (make-next-monkey-fn div monkey-true monkey-false) :ins 0})))
   {}
   (filter #(not-empty (first %)) (partition-by #(= % "") inputs))))

(defn do-round
  [monkeys]
  (reduce
   (fn [_acc id]
     (let [monkey (_acc id)
           items (:items monkey)
           item-count (+ (:ins monkey) (count items))]
       (-> (reduce
            (fn [acc item]
              (let [new-item ((:op monkey) item)
                    next-monkey ((:next monkey) new-item)]
                (update-in acc [next-monkey :items] conj new-item)))
            _acc
            items)
           (assoc-in [id :items] '())
           (assoc-in [id :ins] item-count))))
   monkeys
   (map str (range (count monkeys)))))

(defn get-inspections
  [inputs]
  (map #(:ins %)
       (vals (reduce
              (fn [monkeys _]
                (do-round monkeys))
              (parse-input inputs)
              (repeat 20 nil)))))

(defn run
  [inputs]
  (apply * (take 2 (sort > (get-inspections inputs)))))
