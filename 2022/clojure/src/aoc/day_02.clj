(ns aoc.day-02)

;; A-rock, B-paper, C-scissors, X-rock, Y-paper, Z-scissors
(def score-1 {\A {\X 4 \Y 8 \Z 3}
              \B {\X 1 \Y 5 \Z 9}
              \C {\X 7 \Y 2 \Z 6}})
;; X-loss, Y-draw, Z-win
(def score-2 {\A {\X 3 \Y 4 \Z 8}
              \B {\X 1 \Y 5 \Z 9}
              \C {\X 2 \Y 6 \Z 7}})

(defn input [score]
  (for [[a _ b] (re-seq #".+" (slurp "../day_02.data"))]
    ((score a) b)))

(defn solution-1 []
  (reduce + (input score-1)))

(defn solution-2 []
  (reduce + (input score-2)))

(prn {:one (solution-1) :two (solution-2)})