(ns day-15-fast
  (:require [clojure.data.priority-map :refer [priority-map]]))

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

(defn adj-paths [d risk r paths yx]
  (->> (adjacents d yx)
       (filter #(not (paths %)))
       (map (fn [[y x :as yx]]
              [[y x] (+ r (get-in risk yx))]))))

(defn find-path [d risks paths]
  (let [[yx r] (first paths)
        paths (dissoc paths yx)]
    (if (completed? d yx)
      r
      (let [adjs    (adj-paths d risks r paths yx)
            paths   (reduce
                      (fn [a [k v]] (assoc a k v))
                      paths
                      adjs)]
        (recur d risks paths)))))

(defn +-wrap [& rest]
  (inc (mod (dec (apply + rest)) 9)))

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

;1163751742
;1381373672
;2136511328
;3694931569
;7463417111
;1319128137
;1359912421
;3125421639
;1293138521
;2311944581

;; for better performance we could use a priority queue
;; but then that would be using libraries
(defn run []
  (let [lines  (re-seq #"[^\n]+" (slurp "../day_15.data"))
        risks1 (mapv #(mapv ->int %) lines)
        ;paths  (pq/priority-queue (comp - first) :elements [[0 0 0]])
        paths  (priority-map [0 0] 0)
        risks2 (expand risks1 5)]

    (println "-- Part 1 --")
    (println "Lowest total risk of any path: ")
    (println (find-path (dims risks1) risks1 paths))

    (println "-- Part 2 --")
    (println "Lowest total risk in expanded cave: ")
    ;(println (find-path (dims risks2) risks2 paths))

    ))

;(run)