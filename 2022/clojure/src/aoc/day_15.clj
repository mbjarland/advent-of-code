(ns aoc.day-15)
; rf returns a collection of non-allowed intervals for beacons
(let [d   (partition 2 (partition 2 (map parse-long (re-seq #"(?s)-?\d+" (slurp "data/2022_15")))))
      df  (fn [a b c d] (+ (abs (- a c)) (abs (- b d))))         ; distance function
      bf  #(set (keep (fn [[_ [bx by]]] ({% bx} by)) d))         ; x coords for beacons on row %
      rf  (fn [eb? y]                                            ; row function - eb? - exclude beacons
            (let [zf  (fn [[[sx sy] [bx by]]]                    ; zone function, s - sensor, b - beacon
                        (let [dx (- (df sx sy bx by) (abs (- sy y)))]
                          (when (<= 0 dx) [(- sx dx) (+ sx dx)])))
                  mf  (fn [r [c d]]                              ; merge function
                        (let [[a b] (last r)]
                          (if (and a (<= a c b)) (conj (butlast r) [(min a c) (max b d)])
                            (conj r [c d]))))
                  ef  (fn [[a b]]                                ; exclude beacons function
                        (let [bi (sort (filter #(<= a % b) (bf y)))]
                          (map (fn [[c d]] [(inc c) (dec d)])
                               (partition 2 1 (flatten [(dec a) bi (inc b)])))))
                  res (sort (reduce mf [] (sort (keep zf d))))]
              (if eb? (mapcat ef res) res)))
      xyf (fn [y] (let [f (fn [[[_ a] [b _]]] (map #(vector % y) (range (inc a) b)))]
                    (some identity (mapcat f (partition 2 1 (rf false y))))))]
  (prn {:one (reduce (fn [r [a b]] (+ r (- b a) 1)) 0 (rf true 2000000)) ;(rf 2000000))
        :two (let [[x y] (some xyf (range 0 4000001))] (+ (* x 4000000) y))}))

; {:one 7730280, :two 10382630753392}
; takes about 20s to complete

(comment
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
  )