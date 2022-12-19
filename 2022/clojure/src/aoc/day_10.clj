(ns aoc.day-10
  (:require [clojure.string :refer [join]]))

(let [d (->> (slurp "data/2022_10")
             (re-seq #"noop|-?\d+")
             (map parse-long)
             (mapcat #(if (nil? %) [0] [0 %])))
      r (vec (reductions + 1 d))
      f (fn [a p x] (conj a (if (< (abs (- (mod p 40) x)) 2) \# \.)))]
  (prn {:one (reduce + (map #(* % (r (dec %))) [20 60 100 140 180 220]))
        :two (->> (reduce-kv f [] r) (partition 40) (mapv join))}))