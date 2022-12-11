(ns aoc.day-06)

(defn solve [n]
  (->> (partition n 1 (slurp "../day_06.data"))
       (keep-indexed #(when (apply distinct? %2) (+ n %1)))
       first))

(prn {:one (solve 4) :two (solve 14)})
