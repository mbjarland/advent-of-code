(ns day-01)

(defn increases [ints]
  (count (filter #(apply < %) (partition 2 1 ints))))

(defn sliding-sums [ints]
  (map #(apply + %) (partition 3 1 ints)))

(let [lines  (re-seq #"\d+" (slurp "../day_01.data"))
      ints   (map #(Integer/parseInt %) lines)]
  (println "\n-- Part 1 --")
  (println "Depth measurement increases:"
           (increases ints))

  (println "\n-- Part 2 --")
  (println "Sliding window depth increases: "
           (increases (sliding-sums ints))))
