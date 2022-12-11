(ns aoc.day-05
  (:require [clojure.string :refer [split join split-lines]]))

(defn initial-state [cs]
  (->> (for [line (map vec (butlast (split-lines cs)))]
         (map line (range 1 (count line) 4)))
       (apply mapv vector)
       (mapv #(remove #{\space} %))))

(defn operations [os]
  (partition 3 (map parse-long (re-seq #"\d+" os))))

(defn solve [cfn]
  (let [[is os] (split (slurp "../day_05.data") #"\n\n")]
    (reduce (fn [a [n f t]]
              (let [[s r] (split-at n (a (dec f)))]
                (-> (assoc a (dec f) r)
                    (update (dec t) #(concat (cfn s) %)))))
            (initial-state is)
            (operations os))))

(defn solution-1 []
  (join (map first (solve reverse))))

(defn solution-2 []
  (join (map first (solve identity))))

(prn {:one (solution-1) :two (solution-2)})