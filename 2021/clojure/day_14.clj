(ns day-14
  (:require [clojure.string :refer [join]]
            [clojure.set :refer [map-invert]]))

(defn rules [lines]
  (reduce (fn [a [lr v]] (assoc a lr (first v)))
          (sorted-map)
          (map #(re-seq #"[A-Z]+" %) lines)))

(defn pair-counts [s]
  (reduce
   (fn [a [l r]]
     (update a (str l r) (fnil inc 0)))
   (array-map)
   (partition 2 1 s)))

(defn execute-step
  "executes a simulation step - returns incremented pair counts"
  [rules pc]
  (reduce
   (fn [a [[l r :as lr] v]]
     (let [n (rules lr)]
       (-> a
           (update (str l n) (fnil + 0) v)
           (update (str n r) (fnil + 0) v))))
   (array-map)
   pc))

(defn simulate
  "returns 'pair counts' after steps simulation steps"
  [rules pair-counts steps]
  (reduce
   (fn [pc _] (execute-step rules pc))
   pair-counts
   (range steps)))

(defn char-counts
  "given simulation result pair counts map, returns two character count
  maps for first and last character in a pair respectively"
  [pc]
  (reduce
   (fn [[f s] [[l r] v]]
     [(update f l (fnil + 0) v)
      (update s r (fnil + 0) v)])
   [{} {}]
   pc))

(defn freqs
  "given simulation result (pair count), return character frequencies map
  {<character> <number of times it occurs>}"
  [pc]
  (let [[f s] (char-counts pc)]
    (merge-with max f s)))

(defn run []
  (let [lines  (re-seq #"[^\n]+" (slurp "../day_14.data"))
        pc     (pair-counts (first lines))
        rules  (rules (rest lines))
        answer (fn [f] (- (apply max (vals f)) (apply min (vals f))))]
    (println "-- Part 1 --")
    (println "max - min element frequency after 10 iterations:"
             (answer (freqs (simulate rules pc 10))))
    (println "-- Part 2 --")
    (println "max - min element frequency after 40 iterations:"
             (answer (freqs (simulate rules pc 40))))
    (simulate rules pc 40)
    ))

(run)
