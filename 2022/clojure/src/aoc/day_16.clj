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


;; create a new graph representing


;; create all valid paths of length 30
;; which has the max released pressure?
;; [AA DD]
;; [AA II]
;; [AA BB]
;; [AA nil DD]
;; [AA nil II]
;; [AA nil BB]
;; [AA DD ]

;; rules:
;; 1. when length == 30 - return
;; 2. iff r == 0 never turn on valve
;; 3. iff r not= 0 fork
;; 4. nil in path means valve turned on
;; 5. you are allowed to choose a branch if
;;    you can not find that branch in the path until (the end OR the last nil)
;;

;; note: you also have to remember the flipped valves!

(comment
  (defn search2 [net rwpr seen time path f open score]
    (let [{:keys [r ds]} (net f)
          os (concat (when (and (pos? r) (not (open f))) [(search net
                                                                  rwpr
                                                                  {}
                                                                  (dec time)
                                                                  (conj path nil)
                                                                  f
                                                                  (conj open f)
                                                                  (+ score (* (dec time) r)))])
                     (map #(search net rwpr (assoc seen f true) (dec time) (conj path %) % open score)
                          (filter (complement seen) ds)))]
      (prn :s score :t time :o open :p path)
      (if (or (<= time 1) (empty? os) (= rwpr open))
        {:path path :score score}
        (apply max-key :score os))))
  )

(def its (atom 0))

(defn search [net seen open time f score]
  (swap! its inc)
  (let [room (net f)
        ov   (if (and (pos? (:r room)) (not (open f)))
               (search net {} (assoc open f :t) (dec time) f (+ score (* (dec time) (:r room))))
               0)
        cvs  (map #(search net (assoc seen f :t) open (dec time) % score)
                  (filter (complement seen) (:ds room)))]
    ;(prn :s score :t time :o open)
    (if (or (<= time 1) (empty? cvs))
      (max score ov)
      (apply max ov cvs))))


; c - current, r - rate, d - destinations, t - time remaining, o - open, p - total pressure release
; a - path since last open, rooms - rooms with positive rate
(defn doit []
  (let [l    (map #(re-seq #"[A-Z]{2}|\d+" %) (re-seq #".+" (slurp "data/2022_16")))
        n    (reduce (fn [a [s r & d]] (assoc a s {:r (parse-long r) :ds d}))
                     (sorted-map)
                     l)
        rwpr (set (keep #(when (-> % second :r pos?) (first %)) n))]
    (reset! its 0)
    (prn :rwpr rwpr)
    {:one n :two (search n {} {} 30 "AA" 0) :its @its}))


(comment

  )