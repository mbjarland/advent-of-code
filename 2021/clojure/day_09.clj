(ns day-09
  (:require [clojure.set :refer [map-invert]]
            [clojure.string :refer [join]]))

(def to-int #(Integer/parseInt (str %)))

(defn height-map [lines]
  (let [w    (count (first lines))
        flat (join lines)]
    (reduce
      (fn [acc i]
        (assoc acc [(quot i w) (mod i w)] (to-int (nth flat i))))
      (sorted-map)
      (range (* w (count lines))))))

(defn udrl [[y x]]
  [[(dec y) x] [(inc y) x] [y (dec x)] [y (inc x)]])

(defn basin [hm [k & ks :as b]]
  (into
    (sorted-set)
    (cons k
          (mapcat
            (fn [p]
              (when (and (not= (hm p) 9) (< (hm k) (hm p -1)))
                (basin hm (cons p b))))
            (remove (into #{} b) (udrl k))))))

(defn low-point? [hm k]
  (when
    (every? true? (map #(< (hm k) (hm % 11)) (udrl k)))
    (hm k)))

(defn print-hm [hm basins]
  (let [bold #(str "\033[1m" % "\033[0m")]
    (doseq [[y x :as k] (sort (keys hm))]
      (when (zero? x) (println))
      (print (if (some #(% k) basins) (bold (hm k)) (hm k))))
    (println)
    (println)))

(defn run []
  (let [lines  (re-seq #"[^\n]+" (slurp "../day_09.data"))
        hm     (height-map lines)
        lows   (filter #(low-point? hm %) (keys hm))
        risk   (reduce + (map #(inc (hm %)) lows))
        basins (map #(basin hm [%]) lows)
        large  (take 3 (reverse (sort-by count basins)))]
    (println "\n-- Part 1 --")
    (println "Sum of risk levels of low points: " risk)

    (println "\n-- Part 2 --")
    ;(println "Basins on heat map:")
    ;(print-hm hm basins)
    (println "Product of sizes of three largest basins: "
             (reduce * 1 (map count large)))))

(run)