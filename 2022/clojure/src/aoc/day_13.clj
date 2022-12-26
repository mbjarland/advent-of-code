(ns aoc.day-13)

(let [l  (map read-string (re-seq #".+" (slurp "data/2022_13")))
      f  (fn f [[l & lr] [r & rr]]
           (or (cond (= l r) nil
                     (and (coll? l) (coll? r)) (f l r)
                     (coll? l) (if (nil? r) 1 (f l [r]))
                     (coll? r) (if (nil? l) -1 (f [l] r))
                     :else (compare l r))
               (if (= lr rr) nil (f lr rr))))
      r1 (keep-indexed (fn [i [l r]] (when (= (f l r) -1) (inc i)))
                       (partition 2 l))
      ab #{[[2]] [[6]]}
      r2 (sort f (concat ab l))]
  (prn {:one (reduce + r1)
        :two (reduce * (keep-indexed #(when (ab %2) (inc %1)) r2))}))

;{:one 6272, :two 22288}