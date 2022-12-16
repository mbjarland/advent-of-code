(ns aoc.day-09)

(let [ds    {\L [0 -1] \R [0 1] \U [-1 0] \D [1 0]}
      parse (fn [a [d n]] (concat a (repeat (parse-long n) (ds (first d)))))
      data  (reduce parse [] (partition 2 (re-seq #"\w+" (slurp "data/2022_09"))))
      head  (reductions #(mapv + %1 %2) [0 0] data)
      rope  (fn [[y x :as t] h]
              (let [[dy dx] (map - h t)
                    tx {0 0 2 1 -2 -1}]
                [(+ y (tx dy ({2 dy} (abs dx) 0))) (+ x (tx dx ({2 dx} (abs dy) 0)))]))
      tail  #(reductions rope [0 0] %)]
  (prn {:one (count (distinct (tail head)))
        :two (count (distinct (nth (iterate #(tail %) head) 9)))}))
