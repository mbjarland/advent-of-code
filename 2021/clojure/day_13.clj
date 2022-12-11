(ns day-13)

(def to-int #(Integer/parseInt (str %)))

;8,10
;9,0
;
;fold along y=7
;fold along x=5

(defn parse-fold [str]
  (let [[[_ d n]] (re-seq #"([xy|])=(\d+)" str)]
    {:dir (keyword d) :at (to-int n)}))

(defn parse [lines]
  (let [[xys folds] ((juxt filter remove) #(re-find #"^\d" %) lines)
        xys   (into (sorted-set) (map #(mapv to-int (re-seq #"\d+" %)) xys))
        folds (map parse-fold folds)]
    [xys folds]))

(defn apply-fold [xys {:keys [dir at]}]
  (let [fold-fn (fn [v] (if (< v at) v (- (* 2 at) v)))]
    (into (sorted-set)
          (case dir
                :x (map (fn [[x y]] [(fold-fn x) y]) xys)
                :y (map (fn [[x y]] [x (fold-fn y)]) xys)
                (throw (Exception. (str "invalid fold direction:" dir)))))))

(defn apply-folds [xys folds]
  (reduce
   (fn [acc fold]
     (apply-fold acc fold))
   xys
   folds))

(defn print-board [xys]
  (let [max-x (apply max (map first xys))
        max-y (apply max (map second xys))]
    (doseq [y (range (inc max-y))]
      (doseq [x (range (inc max-x))]
        (print (if (xys [x y]) "#" " ")))
      (println))))


(defn run []
  (let [lines (re-seq #"[^\n]+" (slurp "../day_13.data"))
        [xys folds] (parse lines)
        folded (apply-fold xys (first folds))]
    (println "-- Part 1 --")
    (println "Dots visible after one fold: " (count folded))
    (println "-- Part 2 --")
    (println "Applying all folds gives:")
    (println "---------")
    (print-board (apply-folds xys folds))
    (println "---------")))

(run)