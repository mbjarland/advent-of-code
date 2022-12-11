(ns day-03)

(defn transpose [m]
  (apply mapv vector m))

(defn to-dec [s]
  (reduce #(+ %1 %1 ({\1 1} %2 0)) 0 s))

(defn freqs [lines]
  (->> (transpose lines)
       (map frequencies)
       (map #(merge {\0 0 \1 0} %))))

(defn pos= [str pos val]
  (= (nth str pos) val))

(defn get-rate [freqs op]
  (reduce
    (fn [s f] (str s (if (op (f \0) (f \1)) 0 1)))
    nil
    freqs))

(defn get-rating [lines op]
  (reduce
    (fn [acc pos]
      (let [f   (nth (freqs acc) pos)
            val (if (op (f \1) (f \0)) \1 \0)]
        (if (= (count acc) 1)
          (reduced acc)
          (filter #(pos= % pos val) acc))))
    lines
    (range (count (first lines)))))

(defn run []
  (let [lines   (re-seq #"[^\n]+" (slurp "../day_03.data"))
        freqs   (freqs lines)
        gamma   (get-rate freqs >)
        epsilon (get-rate freqs <)
        oxygen  (first (get-rating lines >=))
        co2     (first (get-rating lines <))]
    (println "\n-- Part 1 --")
    (println "Submarine power consumption:"
             (* (to-dec gamma) (to-dec epsilon)))

    (println "\n-- Part 2 --")
    (println "Submarine life support rating: "
             (* (to-dec oxygen) (to-dec co2)))))

(run)
