(ns day-11
  (:require [clojure.string :refer [join]]))

; maps from step number to number of flashes in that step
(def initial-value {:step    0
                    :flashes (sorted-map)})
(def counter (atom initial-value))

(def to-int #(Integer/parseInt (str %)))

(defn parse-board [lines]
  (mapv #(mapv to-int %) lines))

(defn adjacents [board yx]
  (let [offsets [[-1 -1] [-1 0] [-1 1]
                 [0 -1] [0 1]
                 [1 -1] [1 0] [1 1]]
        result  (map #(map + yx %) offsets)]
    (filter #(get-in board %) result)))                     ; get-in to remove invalid negative/large coords

(defn print-board [desc board]
  (println desc)
  (doseq [row board]
    (println (join (map #(format "%2s" %) row))))
  board)

(defn yxs
  "returns all coordinates [y x] of the argument board"
  [board]
  (for [y (range (count board))
        x (range (count (first board)))]
    [y x]))

(defn update-board
  "calls (ufn board yx) for each cell on the board or for yxs if provided"
  ([board ufn]
   (update-board board ufn (yxs board)))
  ([board ufn yxs]
   (reduce
     (fn [b yx] (ufn b yx))
     board
     yxs)))

(def increment #(update-in %1 %2 inc))

(defn inc-step []
  (swap! counter #(-> (update % :step inc))))

(defn inc-flash []
  (swap! counter #(update-in % [:flashes (:step %)] (fnil inc 0))))

(defn flash [board yx]
  (if (< 9 (get-in board yx) 100)                           ; makes it so that we only flash once
    (let [as (adjacents board yx)]
      (inc-flash)                                           ; flash detected - count it
      (-> board
          (assoc-in yx 100)                                 ; block multiple flashes per step
          (update-board increment as)                       ; inc adjacent cells
          (update-board flash as)))                         ; flash adjacent cells recursively
    board))

(defn reset [board yx]
  (if (> (get-in board yx) 9)
    (assoc-in board yx 0)
    board))

(defn simulate [board steps]
  (reduce
    (fn [b _]
      (inc-step)
      (-> b
          (update-board increment)
          (update-board flash)
          (update-board reset)))
    board
    (range steps)))

(defn first-full-flash []
  (->> (:flashes @counter)
       (filter (fn [[k v]] (when (<= 100 v) k)))
       first
       first))

(defn run []
  (reset! counter initial-value)
  (let [lines  (re-seq #"[^\n]+" (slurp "../day_11.data"))
        board  (parse-board lines)
        result (simulate board 100)]
    (println "\n-- Part 1 --")
    (print-board (str "board after 100 steps:") result)
    (println "Total flashes: " (reduce + 0 (vals (:flashes @counter))))

    (reset! counter initial-value)
    (simulate board 500)
    (println "\n-- Part 2 --")
    (println "First time all octopuses flash: step" (first-full-flash))))

(run)