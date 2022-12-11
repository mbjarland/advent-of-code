import groovy.transform.*


@Field def cfg = config('../day_14.data')

@Field String start = cfg[0]
@Field Map<Character, Integer> ctoi  = cfg[1]
@Field Map<String, Character> rules = cfg[2]

@Field long[] freqs = ctoi.collect { k, v ->
  0
} as long[]
println "ctoi: $ctoi"
println "start: $start freqs: $freqs"
start.toList().collect { it as char }.each { c -> 
  println "c: $c -> ${ctoi[c]}"
  freqs[ctoi[c]]++
}
println "freqs: ${freqs}"

def startTime = System.currentTimeMillis()
int steps = 25
char[] carr = start.toCharArray()

for (int i=0; i<carr.length-1; i++) {
  char l = carr[i]
  char r = carr[i+1]

  println "l: ${l} r: ${r}"
  insert(steps, l, r)
}

Arrays.asList(freqs).withIndex().each { f, i -> 
  def c = ctoi.findResult { k, v -> if (v == i) k else null }
  println "$c -> $f"
}
println freqs

println "class: ${freqs.getClass()} item class: ${freqs[0].getClass()}"

def min = Arrays.asList(freqs).min()
def max = Arrays.asList(freqs).max()

def delta = System.currentTimeMillis()-startTime
println "took: ${delta}ms"
println "answer: ${max - min}"




@CompileStatic
void insert (int depth, 
             char l, 
             char r) {
  if (depth == 0) return

  Character n = rules["" + l + r]
  int i = ctoi[n]
  freqs[i]++
  depth--
  insert depth, l, n
  insert depth, n, r
}

def config (dataPath) {
  def lines = new File(dataPath).text.findAll("[A-Z]+")
  def ctoi = lines.join().toList().collect { 
    it.charAt(0)
  }.unique().sort().withIndex(0).collectEntries { c, i ->
    [c, i]
  }

  def rules = lines.tail().collate(2).collectEntries { l, r ->
    [l, r.charAt(0)]
  }

  def start = lines.first()
  println ctoi
  println rules

  [start, ctoi, rules]
}



/*
(defn config [lines]
  (let [parsed (map #(re-seq #"[A-Z]+" %) lines)
        c->i   (->> (distinct (re-seq #"[A-Z]" (join lines)))
                    (map first)
                    sort
                    (map-indexed #(vector %2 %1))
                    (into (sorted-map)))]
    {:rules (reduce (fn [a [lr v]]
                      (assoc a lr (first v)))
                    (sorted-map)
                    parsed)
     :c->i  c->i}))




(ns day-14
  (:require [clojure.string :refer [join]]
            [clojure.set :refer [map-invert]]))



(defn insert [freqs rules depth l r]
  (reduce
   (fn [f d s]
     (reduce
      (fn []))
     )
   freqs
   (range depth))
  (prn :rules rules)
  (loop [d depth [s freqs] [[[l r]] freqs]]
    (prn :d d :s s)
    (if (zero? d)
      freqs
      (recur
       (dec d)
       (reduce
        (fn [a f [l r]]
          (let [^long new (rules (str l r))]
            [(conj a [l new] [new r])
             (update f new (fnil inc 0))]))
        [[] freqs]
        s)))))


(defn insert2 [freqs rules depth l r]
  (let [fi (fn fi [^longs freqs ^long d ^long l ^long r]
             (if (zero? d)
               freqs
               (let [new   0                           ; ((rules l) r)
                     depth (dec d)]
                 ;(aset-long freqs new (inc (aget freqs new))) ;; inc the relevant pos in the array
                 (fi freqs depth l new)
                 (fi freqs depth new r))))]
    (fi freqs depth l r)))

; (config ["CH -> B"
;          "HH -> N"
;          "CB -> H"])
; ==>
; {:rules {1 {2 0
;             0 2}
;          2 {2 3}}
; :c->i {\B 0
;        \C 1
;        \H 2
;        \N 3}}
;
(defn config [lines]
  (let [parsed (map #(re-seq #"[A-Z]+" %) lines)
        c->i   (->> (distinct (re-seq #"[A-Z]" (join lines)))
                    (map first)
                    sort
                    (map-indexed #(vector %2 %1))
                    (into (sorted-map)))]
    {:rules (reduce (fn [a [lr v]]
                      (assoc a lr (first v)))
                    (sorted-map)
                    parsed)
     :c->i  c->i}))

(defn ->result [{:keys [c->i]} freqs]
  (let [i->c (map-invert c->i)]
    (reduce
     (fn [a [idx n]] (if (pos? n) (assoc a (i->c idx) n) a))
     (sorted-map)
     (map-indexed vector freqs))))


(defn simulate [template {:keys [rules c->i]} steps]
  (let [freqs (frequencies template)
        pairs (map join (partition 2 1 template))]
    ;(prn :c->i c->i :rules rules)
    (doseq [[l r] pairs]
      (insert freqs rules steps l r))
    freqs))

;(defn simulate [template rules steps]
;  (let [freqs  (frequencies template)
;        pairs  (partition 2 1 template)
;        result (map #(grow-recursively {} rules (first %) (second %) steps) pairs)]
;    (apply merge-with + (cons freqs result))))
;
;(defn simulate2 [^String template rules steps]
;  (reduce
;   (fn [a s]
;     (prn :step s :len (.length a))
;     (if (< (.length a) 100) (prn :a a))
;     (execute-step a rules))
;   (StringBuilder. template)
;   (range steps)))

(defn run [steps]
  (let [lines    (re-seq #"[^\n]+" (slurp "../day_14.data"))
        template (first lines)
        config   (config (rest lines))
        ;[min max] (apply (juxt min-key max-key) second (frequencies result1))
        ;_        (prn :running-part-2)
        ;result2  (simulate template rules 40)
        ]

    (prn :result (simulate template config steps))
    (println "")))

;(run)
*/