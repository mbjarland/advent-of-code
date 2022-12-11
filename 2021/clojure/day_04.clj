(ns day-04
  (:require [clojure.string :refer [join]]))

(def to-int #(Integer/parseInt %))

(defn transpose [m]
  (apply map list m))

(defn board [board-lines]
  (map (fn [s] {:nr (to-int s)})
       (re-seq #"[^ ]+" (join " " board-lines))))

(defn win? [board]
  (let [rows     (partition 5 board)
        winning? (fn [quint] (every? true? (map :set quint)))]
    (when (or (some winning? rows)                     ; rows or
              (some winning? (transpose rows)))        ; cols
      board)))

(defn mark-board [board nr]
  (map #(if (= (:nr %) nr)
          (assoc % :set true)
          %)
       board))

(defn play [boards moves let-squid-win?]
  (loop [bs boards [nr & ns] moves]
    (let [marked (map #(mark-board % nr) bs)
          [ws' bs'] ((juxt filter remove) win? marked)]
      (cond
        (and let-squid-win? (empty? bs')) [(mark-board (first bs) nr) nr]
        (and (not let-squid-win?) (not-empty ws')) [(first ws') nr]
        :else (recur bs' ns)))))

(defn score [board nr]
  (* nr (reduce + (map :nr (remove :set board)))))

(defn print-board [board]
  (let [pad  #(format "%2s" %)
        bold #(str "\033[1m" (pad %) "\033[0m")
        fmt  (fn [{:keys [nr set]}] (if set (bold nr) (pad nr)))]
    (doseq [row (partition 5 board)]
      (println (join " " (map fmt row))))))

(defn report [part ordinal board nr]
  (println "\n-- Part" part "--")
  (println ordinal "winning board:")
  (print-board board)
  (println "Winning number:" nr)
  (println "Winning score: " (score board nr)))

(defn run []
  (let [lines  (re-seq #"[^\n]+" (slurp "../day_04.data"))
        moves  (map to-int (re-seq #"[^,]+" (first lines)))
        boards (map board (partition 5 (rest lines)))
        [fwb nr1] (play boards moves false)
        [lwb nr2] (play boards moves true)]
    (report 1 "First" fwb nr1)
    (report 2 "Last" lwb nr2)))

(run)


;(defn play3 [boards moves]
;  (reduce
;   (fn [boards nr]
;     (let [boards  (map #(mark-board % nr) boards)
;           winning (some win? boards)]
;       (if winning
;         (reduced [winning nr])
;         boards)))
;   boards
;   moves))
