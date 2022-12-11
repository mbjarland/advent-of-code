(ns day-07)

(def to-long #(Long/parseLong %))

;TODO: There has got to be a better (analytical/non iterative?) way to do this
; returns a lazy seq of 1 3 6 10 15 21 28 36 45 ...
(def part-2-costs
  (map first (iterate (fn [[a b]] [(+ a b) (inc b)]) [0 1])))

(defn dist [^long a ^long b]                                ;; dist here performance - type hints give x10 boost
  (Math/abs (- a b)))

(defn cost-1 [locs loc]
  (reduce + (map #(dist loc %) locs)))

(defn cost-2 [locs loc]
  (reduce + (map #(nth part-2-costs (dist loc %)) locs)))

(defn best-1 [locs]
  (nth locs (int (/ (count locs) 2))))

(defn run []
  (let [locs       (sort (map to-long (re-seq #"\d+" (slurp "../day_07.data"))))
        candidates (range (first locs) (inc (last locs)))
        loc1       (apply min-key #(cost-1 locs %) candidates)
        loc2       (apply min-key #(cost-2 locs %) candidates)]
    (println "\n-- Part 1 --")
    (println "Best position: " loc1)
    (println "Best position: " (best-1 locs))
    (println "Cost:          " (cost-1 locs loc1))

    (println "\n-- Part 2 --")
    (println "Best position: " loc2)
    (println "Cost:          " (cost-2 locs loc2))))

(run)
