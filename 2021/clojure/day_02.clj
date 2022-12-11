(ns day-02)

(def ->int #(Integer/parseInt %))
(def ->cmd (fn [[d v]] [d (->int v)]))

(defn part-one [[h v a] [dir x]]
  (case dir
    "forward" [(+ h x) v a]
    "down" [h (+ v x) a]
    "up" [h (- v x) a]))

(defn part-two [[h v a] [dir x]]
  (case dir
    "forward" [(+ h x) (+ v (* x a)) a]
    "down" [h v (+ a x)]
    "up" [h v (- a x)]))

(defn drive [cmds act]
  (let [[h v] (reduce act [0 0 0] cmds)]
    (* h v)))

(let [lines (re-seq #"[^\n]+" (slurp "../day_02.data"))
      cmds  (map ->cmd (map #(re-seq #"[^ ]+" %) lines))]
  (println "\n-- Part 1 --")
  (println "Horizontal position * depth: " (drive cmds part-one))

  (println "\n-- Part 2 --")
  (println "Horizontal position * depth: " (drive cmds part-two)))