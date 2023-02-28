(ns aoc.day-11
  (:require [clojure.string :refer [join split]]))

(defn parser [acc [_ & r]]
  (let [is (drop-last 4 r)
        [[o _ & a] d t f] (take-last 4 r)
        a  (when (not= (first a) \o) (parse-long (join a)))]
    (conj acc {:is (vec is)                                 ;; items
               :ac 0                                        ;; activation count
               :dv d                                        ;; divisor
               :op #(({\+ + \* *} o) % (or a %))            ;; operation
               :tx #(if (zero? (mod % d)) t f)})))          ;; transfer

(defn parse []
  (->> (split (slurp "data/2022_11") #"\n\n")
       (map #(re-seq #"[*+] ?(?:\d+|old)|[\d]+" %))
       (map (fn [c] (map #(or (parse-long %) %) c)))
       (reduce parser [])))

(let [data (parse)
      ixs  (range (count data))
      mod* (reduce * (map :dv data))                        ;; limit int sizes, mod with * of dv
      turn (fn [d ms mi]
             (let [{:keys [op tx is]} (nth ms mi)
                   throw (fn [a i]
                           (let [i  (mod (quot (op i) d) mod*)
                                 di (tx i)]
                             (-> (update-in a [mi :is] (comp vec rest))
                                 (update-in [mi :ac] inc)
                                 (update-in [di :is] conj i))))]
               (reduce throw ms is)))
      res  (fn [d] (iterate #(reduce (partial turn d) % ixs) data))]
  (prn :one (->> (nth (res 3) 20) (map :ac) (sort >) (take 2) (reduce *))
       :two (->> (nth (res 1) 10000) (map :ac) (sort >) (take 2) (reduce *))))

