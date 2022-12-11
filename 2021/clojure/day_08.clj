(ns day-08
  (:require [clojure.set :refer [map-invert]]
            [clojure.string :refer [join]]))

; this code actually does more work than necessary - it figures
; out a complete mapping to decode the scrambled output...seemed like more fun
(def digits {"abcefg"  0
             "cf"      1
             "acdeg"   2
             "acdfg"   3
             "bcdf"    4
             "abdfg"   5
             "abdefg"  6
             "acf"     7
             "abcdefg" 8
             "abcdfg"  9})

(defn of-len [signals len]
  (first (filter #(= (count %) len) signals)))

(defn only-left [resolved signal]
  (first (remove resolved signal)))

(defn deduce-mapping
  "given a set of signals (stuff before the pipe) figures out the correct mapping"
  [signals]
  (let [fqs (map-invert (frequencies (join signals)))
        [b e f] [(fqs 6) (fqs 4) (fqs 9)]                   ; bef based on frequencies
        c   (only-left #{f} (of-len signals 2))             ; c - only left in one
        a   (only-left #{c f} (of-len signals 3))           ; a - only left in seven
        d   (only-left #{b c f} (of-len signals 4))         ; d - only left in 4
        g   (only-left #{a b c d e f} "abcdefg")]           ; g - only left
    {a \a b \b c \c d \d e \e f \f g \g}))

(defn parse-line
  "returns a coll<coll<string>, coll<string>> representing one line in the input"
  [line]
  (let [[a b] (re-seq #"[^|]+" line)]
    [(re-seq #"[^ ]+" a) (re-seq #"[^ ]+" b)]))

(defn fix
  "given a mapping and a coll of scrambled out strings - return coll of decoded out strings"
  [mapping out]
  (digits (join (sort (map mapping (sort out))))))

(defn decode
  "given a coll of signals (before pipe) and outputs (after pipe)
   returns the decoded digits (coll of int)"
  [[signals output]]
  (let [mapping (deduce-mapping signals)]
    (map #(fix mapping %) output)))

(defn digits->number [digits]
  (first
    (reduce
      (fn [[a m] d]
        [(+ a (* m d)) (* m 10)]
        )
      [0 1]
      (reverse digits))))

(defn run []
  (let [lines   (map parse-line (re-seq #"[^\n]+" (slurp "../day_08.data")))
        decoded (map decode lines)]
    (println "\n-- Part 1 --")
    (println "Number of times 1, 4, 7, or 8 appear: "
             (count (filter #{1 4 7 8} (flatten decoded))))

    (println "\n-- Part 2 --")
    (println "Sum of all decoded digits in input: "
             (apply + (map digits->number decoded)))))



; A mapping of numbers to segments
;      abcdefg
;    0 abc efg
;    1   c  f  * <- unique length 2
;    2 a cde g
;    3 a cd fg
;    4  bcd f  * <- unique length 4
;    5 ab d fg
;    6 ab defg
;    7 a c  f  * <- unique length 3
;    8 abcdefg * <- unique length 7 (not used)
;    9 abcd fg
;      8687497   <- frequencies
;      abcdefg
;       *  **  <- unique frequencies
;
;    method for extracting mappings:
;
;    we get    since
;    ------    ---------------------------
;    bef    -> unique frequencies (6,4,9)
;    c      -> only one left i 1 (len 2)
;    a      -> only one left in 7 (len 3)
;    d      -> only one left in 4 (len 4)
;    g      -> only one left
;
; example:
; acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf
; sort
; abcdefg bcdef acdfg abcdf abd abcdef bcdefg abef abcdeg ab | bcdef abcdf bcdef abcdf
;
; bcdef -> fgabd -> abdfg -> 5
; abcdf -> cfgad -> acdfg -> 3
;
; 5 3 5 3
;
; g -> e (frequency 4)
; b -> f (frequency 9)
; e -> b (frequency 6)
; a -> c (only left in 1)
; d -> a (diff 1 and 7)
; a -> c (only one left in 1)
; f -> d (only one left in 4)
; c -> g (only not mapped yet)
;



