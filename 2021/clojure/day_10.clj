(ns day-10
  (:require [clojure.string :refer [join]]))

(def braces {\( \), \[ \], \{ \}, \< \>})
(def points {\) 3, \] 57, \} 1197, \> 25137})

(defn invalid
  "if this returns a vector [} )] it means that the line was incomplete,
  if it returns a character it means that the line was corrupted"
  [line]
  (reduce
   (fn [stack c]
     (cond
       (contains? braces c) (conj stack (braces c))
       (= c (peek stack)) (pop stack)
       :else (reduced c)))
   []
   line))

(defn score [incomplete]
  (let [points {\) 1, \] 2, \} 3, \> 4}]
    (reduce
     (fn [a x] (+ (* a 5) (points x)))
     0
     incomplete)))

(defn run []
  (let [lines       (re-seq #"[^\n]+" (slurp "../day_10.data"))
        invalids    (map invalid lines)
        corrupteds  (filter char? invalids)
        incompletes (map reverse (filter #(and (coll? %) (not-empty %)) invalids))
        scores      (sort (map score incompletes))]
    (println "\n-- Part 1 --")
    (println "Syntax error score:" (reduce + (map points corrupteds)))

    (println "\n-- Part 2 --")
    (println "Middlemost score of incomplete lines: "
             (nth scores (int (/ (count scores) 2))))))

(run)
