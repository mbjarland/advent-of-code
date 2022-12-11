(ns aoc.day-04
  (:require [clojure.set :refer [subset?]]))

(defn input []
  (for [line (re-seq #".+" (slurp "data/2022_04"))
        :let [[a b c d] (map parse-long (re-seq #"\d+" line))]]
    [(set (range a (inc b))) (set (range c (inc d)))]))

(defn count-matching [f]
  (count (filter #(apply f %) (input))))

(defn solution-1 []
  (count-matching #(or (subset? %1 %2) (subset? %2 %1))))

(defn solution-2 []
  (count-matching #(or (some %1 %2) (some %2 %1))))

(prn {:one (solution-1) :two (solution-2)})