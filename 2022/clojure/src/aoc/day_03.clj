(ns aoc.day-03)

(def priority
  (merge (zipmap "abcdefghijklmnopqrstuvwxyz" (range 1 27))
         (zipmap "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (range 27 53))))

(def lines (re-seq #".+" (slurp "data/2022_03")))

(defn input-1 []
  (for [line lines
        :let [c (count line)
              h (/ c 2)
              [a b] [(subs line 0 h) (subs line h c)]]]
    (some (set a) b)))

(defn input-2 []
  (for [[a b c] (partition 3 lines)]
    (some (set (filter (set a) b)) c)))

(defn solution-1 []
  (reduce + (map priority (input-1))))

(defn solution-2 []
  (reduce + (map priority (input-2))))

(prn {:one (solution-1) :two (solution-2)})