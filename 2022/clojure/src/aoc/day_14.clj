(ns aoc.day-14
  (:require [clojure.string :refer [join]]))

(def sample (join \newline ["498,4 -> 498,6 -> 496,6"
                            "503,4 -> 502,4 -> 502,9 -> 494,9"]))

(defn print-cave [cave]
  (let [mfn #(apply (juxt min max) (map % cave))
        [min-x max-x] (mfn first)
        [min-y max-y] (mfn second)
        ls  (for [y (range 0 (inc max-y))]
              (join (map #(if (cave [% y]) \# \.) (range min-x (inc max-x)))))]
    (into (sorted-map) (zipmap (range 0 (inc max-y)) ls))))

; pf - parse function, ls - lines, rf - range function, ws - walls
(defn doit []
  (let [parse #(partition 2 1 (partition 2 (map parse-long (re-seq #"\d+" %))))
        lines (map parse (re-seq #".+" (slurp "data/2022_14")))  ;sample))
        rf    #(if (= % %2) (repeat %) (range (min % %2) (inc (max % %2))))
        cave  (set (partition 2 (flatten (for [row lines [[sx sy] [ex ey]] row]
                                           (map vector (rf sx ex) (rf sy ey))))))
        max-y (apply max (map second cave))
        max-x (apply max (map first cave))
        sand  (fn [c] (loop [x 500 y 0]
                        (if-let [[a b] (first (remove c [[x (inc y)] [(dec x) (inc y)] [(inc x) (inc y)]]))]
                          (if (> b (+ max-y 5)) nil (recur a b))
                          (if (= [x y] [500 0]) nil (conj c [x y])))))
        caves #(count (take-while some? (iterate sand %)))
        cave-2 (into cave (map vector (range 0 (* 2 max-x)) (repeat (+ 2 max-y))))]

    (prn {:one (dec (caves cave)) :two (caves cave-2)})))

;(prn {:one lines
;      :two cave
;      :max-y max-y}))

;{:one 6272, :two 22288}