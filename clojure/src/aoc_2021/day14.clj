(ns aoc-2021.day14
  "Extended Polymerization - I admit I took a peek at others' solutions to realize that the key was to
  count pairs, not build up the actual final polymer string."
  (:require
   [clojure.java.io :as io]
   [debugger :refer [dbg]]))

(defn parse-rule
  "Given a line of input representing a pair insertion rule, which is a string that looks like 'CB ->
  H', return a map of first string CB to vector of remaining Strings [CH HB]"
  [rule-str]
  (let [[i j] (take 2 rule-str)
        k (last rule-str)]
    {(str i j) [(str i k) (str k j)]}))

(defn initial-polymer-map
  "Turn the initial polymer string like 'NNCB' into a map of pairs each with an initial count of
  1, like {'NN' 1 'NC' 1 'CB' 1}"
  [input-str]
  ;; stoopid the real template is longer than four
  #_(let [[s1 s2 s3 s4] (seq input-str)]
    {(str s1 s2) 1 (str s2 s3) 1 (str s3 s4) 1})
  ;; stoooopid! I assumed there were no initial duplicate pairs
  #_(let [elements (seq input-str)]
    (apply assoc {} (interleave 
                     (->> elements
                          (partition-all 2 1)
                          butlast
                          (map (partial apply str)))
                     (repeat 1))))
  (let [elements (seq input-str)]
    (->> elements
         (partition-all 2 1)
         butlast
         (map (partial apply str))
         frequencies)))

(defn step
  "'Grow' the polymer by one step. Given the expansion rules and the current count of pairs, produce
  the next count of pairs."
  [rules-m counts-m]
  (let [f (fn [m [pair-str pair-count]]
            (reduce (fn [m' pair-str']
                      (update m' pair-str' (fnil (partial + pair-count) 0))) m (rules-m pair-str))
            )]
    (reduce f {} counts-m)))

(defn kersplode
  "Given the map of pair frequencies (where keys are two-character Strings), return a map of
  frequencies of the individual characters comprising all of the strings"
  [m]
  (let [char-counts (reduce (fn [m' [chars n]]
                              (reduce (fn [m' c1]
                                        (update m' c1 (fnil (partial + n) 0))) m' chars)) {} m)
        adjust-for-duplicates (fn [n]
                                (if (odd? n)
                                  (inc (/ (dec n) 2))
                                  (/ n 2)))]
    (reduce (fn [m [k v]] (assoc m k (adjust-for-duplicates v))) char-counts char-counts)))

(defn runner
  "Parse the initial input. `iterate` the `step` function `i` times. Count the individual elements,
  sort by frequencies low to high, return the diff of highest and lowest frequency."
  [i input]
  (let [counts-m (initial-polymer-map (first input))
        rules-m (apply merge (map parse-rule (drop 2 input)))
        counts-m' (first (drop i (iterate (partial step rules-m) counts-m)))
        sorted (sort-by second (kersplode counts-m'))]
    (- (-> sorted last second) (-> sorted first second))))


(comment

  ;; pt 2

  (runner 40 ["NNCB"
              ""
              "CH -> B"
              "HH -> N"
              "CB -> H"
              "NH -> C"
              "HB -> C"
              "HC -> B"
              "HN -> C"
              "NN -> C"
              "BH -> H"
              "NC -> B"
              "NB -> B"
              "BN -> B"
              "BB -> N"
              "BC -> B"
              "CC -> N"
              "CN -> C"]) ;; 2188189693529

  (with-open [r (io/reader (io/resource "aoc-2021/day14.txt"))]
    (runner 40 (line-seq r))) ;; 3572761917024

  ;; pt 1

  (runner 10 ["NNCB"
              ""
              "CH -> B"
              "HH -> N"
              "CB -> H"
              "NH -> C"
              "HB -> C"
              "HC -> B"
              "HN -> C"
              "NN -> C"
              "BH -> H"
              "NC -> B"
              "NB -> B"
              "BN -> B"
              "BB -> N"
              "BC -> B"
              "CC -> N"
              "CN -> C"]) ;; 1588
                          ;; ([\H 161] [\C 298] [\N 865] [\B 1749])
                          ;; ([\H 322] [\C 596] [\N 1729] [\B 3497])
                          ;;      161      298      864.5     1748.5 
     ;; frequenies should be: B 1749 C 298 H 161 N 865
     ;; ([\H 236] [\C 304] [\N 843] [\B 1689])

  (with-open [r (io/reader (io/resource "aoc-2021/day14.txt"))]
    (runner 10 (line-seq r))) ;; 2988   ;; 2781 too low

  (parse-rule "NC -> B")
  (initial-polymer-map "NNCB")

  (step 
   {"CH" '("CB" "BH"), "HH" '("HN" "NH"), "BH" '("BH" "HH"), "BN" '("BB" "BN"), "NH" '("NC" "CH"), "NB" '("NB" "BB"), "HB" '("HC" "CB"), "BC" '("BB" "BC"), "CN" '("CC" "CN"), "CC" '("CN" "NC"), "BB" '("BN" "NB"), "CB" '("CH" "HB"), "HN" '("HC" "CN"), "HC" '("HB" "BC"), "NC" '("NB" "BC"), "NN" '("NC" "CN")}
   {"NN" 1, "NC" 1, "CB" 1} )

  (nth (iterate (partial 
             step 
             {"CH" '("CB" "BH"), "HH" '("HN" "NH"), "BH" '("BH" "HH"), "BN" '("BB" "BN"), "NH" '("NC" "CH"), "NB" '("NB" "BB"), "HB" '("HC" "CB"), "BC" '("BB" "BC"), "CN" '("CC" "CN"), "CC" '("CN" "NC"), "BB" '("BN" "NB"), "CB" '("CH" "HB"), "HN" '("HC" "CN"), "HC" '("HB" "BC"), "NC" '("NB" "BC"), "NN" '("NC" "CN")})
            {"NN" 1, "NC" 1, "CB" 1} ) 3)

  (kersplode {"CH" 2, "HH" 1, "BH" 1, "BN" 2, "NB" 4, "HB" 3, "BC" 3, "CN" 2, "CC" 1, "BB" 4, "NC" 1}) 

  (sort-by second {\C 10, \H 8, \B 21, \N 9})

  (update {} "N" (partial (fnil + 0 0) 3))

  (update {} "N" (fnil (partial + 3) 0))

  (frequencies "VFHKKOKKCPBONFHNPHPN") ;; {\B 1, \C 1, \F 2, \H 3, \K 4, \N 3, \O 2, \P 3, \V 1}
  (initial-polymer-map "VFHKKOKKCPBONFHNPHPN") ;; {"KC" 1, "KO" 1, "KK" 2, "NF" 1, "FH" 2, "VF" 1, "ON" 1, "BO" 1, "NP" 1, "PN" 1, "OK" 1, "PH" 1, "CP" 1, "HN" 1, "PB" 1, "HP" 1, "HK" 1}
  (kersplode {"KC" 1, "KO" 1, "KK" 2, "NF" 1, "FH" 2, "VF" 1, "ON" 1, "BO" 1, "NP" 1, "PN" 1, "OK" 1, "PH" 1, "CP" 1, "HN" 1, "PB" 1, "HP" 1, "HK" 1}) ;; {\B 1, \C 1, \F 2, \H 3, \K 4, \N 3, \O 2, \P 3, \V 1}

  (frequencies (map (partial apply str) (butlast (partition-all 2 1 [1 2 3 4 2 3]))))

  )
