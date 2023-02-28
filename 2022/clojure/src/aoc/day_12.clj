(ns aoc.day-12)
; s - start yx, e - end yx, d - map yx->height, c - cost, w - wave, ns - neighbours
(let [m     (zipmap "SEabcdefghijklmnopqrstuvwxyz" (concat [0 25] (range)))
      l     (re-seq #".+" (slurp "data/2022_12"))
      mx    (count (first l))
      parse #(let [yx ((juxt quot mod) %2 mx)]
               (cond-> (assoc-in %1 [0 yx] (m %3))
                       (= \S %3) (assoc 1 yx)
                       (= \E %3) (assoc 2 yx)))
      [d s e] (reduce-kv parse [] (vec (apply str l)))
      cf    (fn [c d w]
              (let [ns (fn [[y x :as yx]]
                         (filter #(and (not (w %)) (d %) (<= (d %) (inc (d yx))))
                                 [[(inc y) x] [(dec y) x] [y (inc x)] [y (dec x)]]))]
                (cond (w e) c
                      (empty? w) Long/MAX_VALUE
                      :else (recur (inc c) (reduce dissoc d w) (set (mapcat ns w))))))
      cost  #(cf 0 d #{%})]
  (prn {:one (cost s) :two (apply min (map cost (filter (comp zero? d) (keys d))))}))

;{:one 350, :two 349}