(ns day-06
  (:require [clojure.set :refer [rename-keys]]))

(def to-int #(Integer/parseInt %))

(defn school [ints]
  (reduce
   (fn [a [k v]] (assoc a k v))
   (apply sorted-map (interleave (range 9) (repeat 0)))
   (frequencies ints)))

(defn age [fish d]
  (-> (rename-keys fish (zipmap (range 1 9) (range 8)))
      (update 6 #(+ % (fish 0)))
      (assoc 8 (fish 0))))

(defn simulate [fish days]
  (apply + (vals (reduce age fish (range days)))))

(defn run []
  (let [ints (map to-int (re-seq #"\d+" (slurp "../day_06.data")))
        fish (school ints)]
    (println "\n-- Part 1 --")
    (println "Number of fish after 80 days: " (simulate fish 80))

    (println "\n-- Part 2 --")
    (println "Number of fish after 256 days:" (simulate fish 256))))

(run)
