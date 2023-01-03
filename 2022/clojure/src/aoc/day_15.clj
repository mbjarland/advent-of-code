(ns aoc.day-15
  (:require [clojure.string :refer [join]]))

(def sample (join \newline ["Sensor at x=2, y=18: closest beacon is at x=-2, y=15"

                            "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
                            "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
                            "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
                            "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
                            "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
                            "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
                            "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
                            "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
                            "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
                            "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
                            "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
                            "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
                            "Sensor at x=20, y=1: closest beacon is at x=15, y=3"

                            ]))
;                   sf (fn [[n m]] (or (<= n g m) (<= n h m) (<= g n h) (<= g m h)))
;                   [c d] (first (filter sf r))
; pf - parse function, ls - lines, rf - range function, ws - walls
(defn doit []
  (let [d  (partition 2 (partition 2 (map parse-long (re-seq #"(?s)-?\d+" (slurp "data/2022_15")))))
        df (fn [[a b] [c d]] (+ (abs (- a c)) (abs (- b d))))
        rf (fn [y]
             (let [if (fn [r [[sx sy :as s] b]]                  ; s - sensor, b - beacon, if - interval function
                        (let [dx (- (df s b) (abs (- sy y)))]
                          (if (<= 0 dx) (conj r [(- sx dx) (+ sx dx)]) r)))
                   mf (fn [r [c d]]                              ; mf - merge function
                        (let [[a b] (last r)]
                          (if (and a (<= a c b))
                            (conj (butlast r) [(min a c) (max b d)]) (conj r [c d]))))
                   bx (set (keep (fn [[[_ _] [bx by]]] (when (= by y) bx)) d))
                   ;bx (set (keep #(when (= y (nth (nth % 1) 1)) (nth (nth % 1) 0)) d))

                   bf (fn [[a b]]
                        (let [bi (sort (filter #(<= a % b) bx))]
                          (map (fn [[c d]] [(inc c) (dec d)])
                               (partition 2 1 (flatten [(dec a) bi (inc b)])))))]
               (->> (sort (reduce if #{} d))
                    (reduce mf [])
                    (mapcat bf))))]
    ;(prn :d d)
    (prn {:one (reduce (fn [r [a b]] (+ r (- b a))) 0 (rf 2000000))
          :two nil})
    ;(prn {:one (count (rf 10)) :two nil})

    ))

(comment
  (let [xr #(map (fn [[[sx _] [bx _]]] (%1 sx (abs (- sx bx)))) d)
        [lx hx] [(apply min (xr -)) (apply max (xr +))]
        ]))