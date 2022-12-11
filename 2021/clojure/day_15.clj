(ns day-15)

(def ->int #(Integer/parseInt (str %)))

(defn dims [m]
  [(count m) (count (first m))])

(defn within? [[my mx] [y x]]
  (and (< -1 y my) (< -1 x mx)))

(defn adjacents [d [y x]]
  (filter #(within? d %)
          [[(dec y) x] [(inc y) x] [y (dec x)] [y (inc x)]]))

(defn completed? [[my mx] [y x]]
  (and (= y (dec my))
       (= x (dec mx))))

(defn adj-paths [d r risk visited yx]
  (->> (adjacents d yx)
       (filter #(not (visited %)))
       (map (fn [[y x :as yx]]
              [(+ r (get-in risk yx)) y x]))))

(defn find-path [d risks paths visited]
  (let [[r & yx :as ryx] (first paths)]
    (if (completed? d yx)
      r
      (let [visited (assoc visited yx true)
            adjs    (adj-paths d r risks visited yx)
            paths   (into (disj paths ryx) adjs)]
        (recur d risks paths visited)))))

(defn +-wrap [& rest]
  (let [s (apply + rest)]
    (if (< 9 s) (mod s 9) s)))

(defn expand [risks n]
  (let [[my mx] (dims risks)]
    (reduce
      (fn [a off]
        (let [[bi ri] ((juxt quot mod) off my)]
          (conj a (mapv +-wrap
                        (flatten (repeat n (risks ri)))
                        (flatten (iterate #(map inc %) (repeat mx 0)))
                        (repeat bi)))))
      []
      (range (* my n)))))

;; for better performance we could use a priority queue
;; but then that would be using libraries
(defn run []
  (let [lines  (re-seq #"[^\n]+" (slurp "../day_15.data"))
        risks1 (mapv #(mapv ->int %) lines)
        paths  (sorted-set [0 0 0])
        risks2 (expand risks1 5)]

    (println "-- Part 1 --")
    (print "Lowest total risk of any path: ")
    (println (find-path (dims risks1) risks1 paths {}))

    (println "-- Part 2 --")
    (print "Lowest total risk in expanded cave: ")
    (println (find-path (dims risks2) risks2 paths {}))

    ))

;(time (run))