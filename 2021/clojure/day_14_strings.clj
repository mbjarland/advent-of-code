(ns day-14
  (:require [clojure.string :refer [join]]))


(defn insert [rules freqs depth l r]
  (if (zero? depth)
    freqs
    (let [val (rules ())]))

  )


(defn execute-step [^StringBuilder template rules]
  (let [len (.length template)]
    (loop [i 0]
      (prn :i i :len len)
      (if (> i len)
        template
        (let [pair (.substring template i (+ i 2))]
          (.insert template (inc i) (rules pair))
          (recur (+ i 2)))))))

(defn execute-step2 [template rules]
  (reduce
   (fn [acc [_ r :as pair]]
     (str acc (rules pair) r))
   (first template)
   (map join (partition 2 1 template))))

(defn grow-recursively [freqs rules a b max-depth]
  (if (zero? max-depth)
    freqs
    (let [new    (rules (str a b))
          _      (prn :new new)
          freqs' (update freqs new (fnil inc 0))]
      (prn :gr :a a :b b :d max-depth :freqs freqs' :rules rules)
      (merge-with +
                  (grow-recursively freqs' rules a new (dec max-depth))
                  (grow-recursively freqs' rules new b (dec max-depth))))))

(defn simulate [template rules steps]
  (let [freqs  (frequencies template)
        pairs  (partition 2 1 template)
        result (map #(grow-recursively {} rules (first %) (second %) steps) pairs)]
    (apply merge-with + (cons freqs result))))

(defn simulate2 [^String template rules steps]
  (reduce
   (fn [a s]
     (prn :step s :len (.length a))
     (if (< (.length a) 100) (prn :a a))
     (execute-step a rules))
   (StringBuilder. template)
   (range steps)))

(defn run []
  (let [lines    (re-seq #"[^\n]+" (slurp "../day_14.data"))
        template (first lines)
        rules    (->> (rest lines)
                      (map #(re-seq #"[A-Z]+" %))
                      (mapcat (fn [[k v]]
                                (prn :k k :v (first v))
                                [k (first v)]))
                      (apply array-map))
        _        (prn :running-part-1)
        result1  (simulate template rules 1)
        ;[min max] (apply (juxt min-key max-key) second (frequencies result1))
        ;_        (prn :running-part-2)
        ;result2  (simulate template rules 40)
        ]
    (prn :freqs result1)
    (prn :min min :max max :mul (- (second max) (second min)))
    ;(prn :rules rules)
    ;(prn :example (execute-step "CPC" rules))
    (println "-- Part 1 --")
    (println "After 10 steps, most common - least common:"
             (- (second max) (second min)))

    (println "-- Part 2 --")
    (println "")))

;(run)
