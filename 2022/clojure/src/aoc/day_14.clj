(ns aoc.day-14)
; pf - parse function, ls - lines, rf - range function
(let [parse  #(partition 2 1 (partition 2 (map parse-long (re-seq #"\d+" %))))
      lines  (map parse (re-seq #".+" (slurp "data/2022_14")))
      rf     #(if (= % %2) (repeat %) (range (min % %2) (inc (max % %2))))
      cave-1 (set (partition 2 (flatten (for [row lines [[sx sy] [ex ey]] row]
                                          (map vector (rf sx ex) (rf sy ey))))))
      mx     (apply max (map second cave-1))
      my     (apply max (map first cave-1))
      cave-2 (into cave-1 (map vector (range 0 (* 2 my)) (repeat (+ 2 mx))))
      sand   (fn [c] (loop [x 500 y 0]
                       (if-let [[a b] (first (remove c [[x (inc y)] [(dec x) (inc y)] [(inc x) (inc y)]]))]
                         (if (> b (+ mx 5)) nil (recur a b))
                         (if (= [x y] [500 0]) nil (conj c [x y])))))
      units  #(count (take-while some? (iterate sand %)))]
  (prn {:one (dec (units cave-1)) :two (units cave-2)}))
;{:one 808, :two 26625}

(comment
  (defn print-cave [cave]
    (let [mfn #(apply (juxt min max) (map % cave))
          [min-x max-x] (mfn first)
          [min-y max-y] (mfn second)
          ls  (for [y (range 0 (inc max-y))]
                (join (map #(if (cave [% y]) \# \.) (range min-x (inc max-x)))))]
      (into (sorted-map) (zipmap (range 0 (inc max-y)) ls))))
  )
