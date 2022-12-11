(ns adventofcode-2022.exercises.day-11.part-2)

(defn item-or-n
  [item n]
  (if (= "old" n) item (Integer/parseInt n)))

(defn make-op-fn
  [[left op right]]
  (fn [item]
    (let [parsed-left (item-or-n item left)
          parsed-right (item-or-n item right)]
      ((resolve (symbol op)) parsed-left parsed-right))))

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
       (assoc acc 
              id {:items items :op (make-op-fn op) :next (make-next-monkey-fn div monkey-true monkey-false) :ins 0}
              :modulus (* (:modulus acc) div))))
   {:modulus 1}
   (filter #(not-empty (first %)) (partition-by #(= % "") inputs))))

(defn do-round
  [monkeys modulus]
  (reduce
   (fn [_acc id]
     (let [monkey (_acc id)
           items (:items monkey)
           item-count (+ (:ins monkey) (count items))]
       (-> (reduce
            (fn [acc item]
              (let [new-item (rem ((:op monkey) item) modulus)
                    next-monkey ((:next monkey) new-item)]
                (update-in acc [next-monkey :items] conj new-item)))
            _acc
            items)
           (assoc-in [id :items] '())
           (assoc-in [id :ins] item-count))))
   monkeys
   (map str (range (count monkeys)))))

(defn get-inspections
  [monkeys modulus]
  (map #(:ins %)
       (vals (reduce
              (fn [acc r]
                (do-round acc modulus))
              monkeys
              (range 10000)))))

(defn run
  [inputs]
  (let [parsed-inputs (parse-input inputs)
        modulus (:modulus parsed-inputs)
        monkeys (dissoc parsed-inputs :modulus)]
    (apply * (take 2 (sort > (get-inspections monkeys modulus))))))
