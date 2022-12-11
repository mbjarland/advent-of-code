(ns day-12
  (:require [clojure.set :refer [difference]]
            [clojure.pprint :refer [pprint]]))

(defn small? [pos]
  (Character/isLowerCase ^char (first (name pos))))

(defn pair [line]
  (let [[f t] (re-seq #"\w+" line)]
    [(keyword f) (keyword t)]))

;; node comparator - not required but makes it so that
;; we always get :start at the beginning of lists/maps
;; and :end at the end...easier to read
(defn node-cmp [a b]
  (cond
    (= a b) 0
    (= a :start) -1
    (= b :start) 1
    (= a :end) 1
    (= b :end) -1
    :else (compare (.toLowerCase (name a))
                   (.toLowerCase (name b))))) \
; graphviz generator, totally not required, was curious
; how the map of the caves would look when rendered graphically
(defn graphviz [pairs]
  (str "graph {"
       (reduce
        (fn [a [f t]] (str a \newline "  " (name f) "--" (name t)))
        ""
        pairs)
       \newline
       "}"))

(defn cave-map
  "reads the pairs into a map {from [possible tos]}"
  [pairs]
  (let [add (fnil conj (sorted-set-by node-cmp))]
    (reduce
     (fn [acc [f t]]
       (-> acc
           (update f #(add %1 t))
           (update t #(add %1 f))))
     (sorted-map-by node-cmp)
     pairs)))

(defn small-visited-twice? [path]
  (some #(< 1 %) (vals (frequencies (filter small? path)))))

(defn find-paths
  "solves the problem - recursively walks rooms using to cave map"
  [cave-map path part-2?]
  (let [here   (last path)
        smalls (into #{} (filter small? path))
        exits  (difference (cave-map here) smalls)
        exits  (if (and part-2? (not (small-visited-twice? path)))
                 (remove #{:start} (cave-map here))
                 exits)]
    (if (= here :end)
      [path]
      (reduce
       (fn [acc to]
         (concat acc (find-paths cave-map (conj path to) part-2?)))
       []
       exits))))

(defn run []
  (let [lines (re-seq #"[^\n]+" (slurp "../day_12.data"))
        pairs (map pair lines)
        map   (cave-map pairs)
        paths (fn [part-2?] (find-paths map [:start] part-2?))]
    (println "\nmap:")
    (pprint map)
    (println "\ngraphviz:")
    (println "render at: https://dreampuf.github.io/GraphvizOnline/")
    (println (graphviz pairs))
    ;(println "\npaths:")
    ;(doseq [p (sort paths)] (prn p))
    (println "\n-- Part 1 --")
    (println "Found" (count (paths false)) "paths through the caves!")
    (println "\n-- Part 2 --")
    (println "Found" (count (paths true)) "paths through the caves!")
    ;(println "\npaths:")
    ;(doseq [p (sort (paths true))] (prn p))
    ))

(run)

(comment
 ;; a map
 {:start #{:A :b}
  :A     #{:start :b :c :end}
  :b     #{:start :A :d :end}
  :c     #{:A}
  :d     #{:b}
  :end   #{:A :b}}
 )

