(ns aoc.day-07)

; could not resist a rewrite - thank you @Apple
(defn parser [state [_ cmd path size-str]]
  (if (= "cd" cmd)
    (if (= path "..")
      (update state :path pop)
      (update state :path conj path))
    (let [size (parse-long size-str)]
      (reduce #(update-in %1 [:size %2] (fnil + 0) size)
              state
              (rest (reductions conj [] (:path state)))))))

(defn parser [state [_ cmd path size-str]]
  (if (= "cd" cmd)
    (if (= path "..")
      (update state :path pop)
      (update state :path conj path))
    (let [size (Long/parseLong size-str)]
      (update-in state [:size]
                 (fnil (partial reduce + 0) size)
                 (rest (reductions conj [] (:path state)))))))

(defn parse []
  (->> (slurp "../day_07.data")
       (re-seq #"\$ (cd) (.+)|(\d+)")
       (reduce parser {:path []})
       :size vals sort reverse))

(let [[t & r :as p] (parse)
      s1 (reduce + (filter #(< % 1e5) p))
      s2 (last (take-while #(> % (- t 4e7)) r))]
  (prn {:one s1 :two s2}))
