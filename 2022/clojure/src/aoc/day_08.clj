(ns aoc.day-08
  (:require [clojure.string :refer [split]]))

(let [nd    (mapv #(mapv (comp parse-long str) %) (split (slurp "data/2022_08") #"\n"))
      td    (apply mapv vector nd)
      [my mx] [(dec (count nd)) (dec (count td))]
      views (fn [y x] [(reverse (take x (nd y))) (drop (inc x) (nd y))
                       (reverse (take y (td x))) (drop (inc y) (td x))])
      yxvs  (for [y (range 1 my) x (range 1 mx)] [y x ((nd y) x)])
      fn1   (fn [a [y x v]]
              (if (some #(every? (partial > v) %) (views y x))
                (inc a) a))
      fn2   (fn [a [y x v]]
              (let [dist #(if (< %2 v) (inc %) (reduced (inc %)))]
                (max a (->> (views y x)
                            (map #(reduce dist 0 %))
                            (reduce *)))))]
  (prn {:one (reduce fn1 (+ (* 2 my) (* 2 mx)) yxvs)
        :two (reduce fn2 0 yxvs)}))
;{:one 1785, :two 345168}

(comment
  ;; nice alternative by @nooga - revised by @Apple
  (let [d        (->> (slurp "data/2022_08")
                      (re-seq #"[^\n]+")
                      (map #(->> (re-seq #"\d" %) (map parse-long))))
        seen     (fn [row]
                   (loop [[h & t] row, top -1, r []]
                     (if h
                       (recur t (max top h) (conj r (> h top)))
                       r)))
        scenic   (fn [row]
                   (loop [[h & t] row, r []]
                     (if h
                       (recur t
                              (let [l (count (take-while #(< % h) t))]
                                (conj r (if (= l (count t)) l (inc l)))))
                       r)))
        four-way (fn [comb f]
                   (let [rowfn (fn [row] (map comb (f row) (reverse (f (reverse row)))))]
                     (mapcat (partial map comb)
                             (map rowfn d)
                             (apply map list (map rowfn (apply map list d))))))]
    [(->> (four-way #(or % %2) seen) (filter true?) count)
     (->> (four-way * scenic) (reduce max))])

  )