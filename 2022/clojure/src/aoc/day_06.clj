(ns aoc.day-06)

(defn solve [n]
  (->> (partition n 1 (slurp "data/2022_06"))
       (keep-indexed #(when (apply distinct? %2) (+ n %1)))
       first))

(prn {:one (solve 4) :two (solve 14)})
