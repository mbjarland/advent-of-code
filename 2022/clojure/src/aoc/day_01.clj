(ns aoc.day-01
  (:require [clojure.string :as str]))

(defn input []
  (for [elf (str/split (slurp "data/2022_01") #"\n\n")]
   (reduce + (map parse-long (re-seq #"\d+" elf)))))

(defn solution-1 []
  (apply max (input)))

(defn solution-2 []
  (reduce + (take-last 3 (sort (input)))))

(prn {:one (solution-1) :two (solution-2)})
