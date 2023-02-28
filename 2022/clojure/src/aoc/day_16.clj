(ns aoc.day-16
  (:require [clojure.string :refer [join]]))

(def sample (join \newline ["Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
                            "Valve BB has flow rate=13; tunnels lead to valves CC, AA"
                            "Valve CC has flow rate=2; tunnels lead to valves DD, BB"
                            "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE"
                            "Valve EE has flow rate=3; tunnels lead to valves FF, DD"
                            "Valve FF has flow rate=0; tunnels lead to valves EE, GG"
                            "Valve GG has flow rate=0; tunnels lead to valves FF, HH"
                            "Valve HH has flow rate=22; tunnel leads to valve GG"
                            "Valve II has flow rate=0; tunnels lead to valves AA, JJ"
                            "Valve JJ has flow rate=21; tunnel leads to valve II"]))

;; create all valid paths of length 30
;; which has the max released pressure?
;; [AA DD]
;; [AA II]
;; [AA BB]
;; [AA nil DD]
;; [AA nil II]
;; [AA nil BB]
;; [AA DD ]
; c - current, r - rate, d - destinations, t - time remaining, o - open, p - total pressure release
; a - path since last open

(defn gen-all-combos [])

(defn doit []
  (let [l (map #(re-seq #"[A-Z]{2}|\d+" %) (re-seq #".+" sample))
        m (reduce (fn [a [s r & d]] (assoc a s {:r (parse-long r) :d d})) {} l)
        rf (fn [p] (loop [r 0 t 30 [a b & ps :as pps] p]
                     (cond (= 0 t) r
                           (nil? b) (recur (+ r (* (dec t) (:r (m a)))) (- t 2) ps)
                           :else (recur r (dec t) (rest pps)))))]
    {:one m :two l}))