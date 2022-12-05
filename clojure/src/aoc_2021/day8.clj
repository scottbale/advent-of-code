(ns aoc-2021.day8
  "Seven Segment Search"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as c.set]
   [clojure.string :as s]
   [debugger :refer [dbg]]))

(defn runner-pt1
  ""
  [input]
  (let [input (map (comp last #(s/split % #" \| ")) input)
        input (mapcat #(s/split % #" ") input)
        input (map count input)
        keeps #{2 3 4 7}]
    (count (filter keeps input ))))

;; rules - each fn takes a state and returns an updated state
;; state is a map with these keywords:
;; `lookup` is the lookup map being built: set->digit
;; `reverse-lookup` is `lookup` inverted: digit->set
;; `remaining` is the ordered list of sets remaining to be identified
;; the returned state should increase the map sizes by one and decrease the `remaining` size by one

(defn rule-6
  "6 is the set of size six which does not contain all of set 7"
  [{:keys [lookup reverse-lookup remaining]}]
  (let [seven (reverse-lookup 7)
        filterer (fn [a-set]
                   (and
                    (== 6 (count a-set))
                    (not (c.set/subset? seven a-set))))
        six (first (filter filterer remaining))
        lookup (assoc lookup six 6)
        remaining (remove (partial identical? six) remaining)]
    {:lookup lookup
     :reverse-lookup (c.set/map-invert lookup)
     :remaining remaining}))

(defn rule-9
  "9 is the set of size six which contains all of set 4"
  [{:keys [lookup reverse-lookup remaining]}]
  (let [four (reverse-lookup 4)
        filterer (fn [a-set]
                   (and
                    (== 6 (count a-set))
                    (c.set/subset? four a-set)))
        nine (first (filter filterer remaining))
        lookup (assoc lookup nine 9)
        remaining (remove (partial identical? nine) remaining)]
    {:lookup lookup
     :reverse-lookup (c.set/map-invert lookup)
     :remaining remaining}))

(defn rule-0
  "0 is the remaining set of size six"
  [{:keys [lookup reverse-lookup remaining]}]
  (let [filterer (fn [a-set]
                   (== 6 (count a-set)))
        zero (first (filter filterer remaining))
        lookup (assoc lookup zero 0)
        remaining (remove (partial identical? zero) remaining)]
    {:lookup lookup
     :reverse-lookup (c.set/map-invert lookup)
     :remaining remaining}))

(defn rule-3
  "3 is the set of size five containing all of set 1"
  [{:keys [lookup reverse-lookup remaining]}]
  (let [one (reverse-lookup 1)
        filterer (fn [a-set]
                   (and
                    (== 5 (count a-set))
                    (c.set/subset? one a-set)))
        three (first (filter filterer remaining))
        lookup (assoc lookup three 3)
        remaining (remove (partial identical? three) remaining)]
    {:lookup lookup
     :reverse-lookup (c.set/map-invert lookup)
     :remaining remaining}))

(defn rule-5
  "5 is the set of size five which set 6 contains all of"
  [{:keys [lookup reverse-lookup remaining]}]
  (let [six (reverse-lookup 6)
        filterer (fn [a-set]
                   (and
                    (== 5 (count a-set))
                    (c.set/superset? six a-set)))
        five (first (filter filterer remaining))
        lookup (assoc lookup five 5)
        remaining (remove (partial identical? five) remaining)]
    {:lookup lookup
     :reverse-lookup (c.set/map-invert lookup)
     :remaining remaining}))

(defn rule-2
  "2 is the remaining set (of size five)"
  [{:keys [lookup reverse-lookup remaining]}]
  (let [two (first remaining)
        lookup (assoc lookup two 2)]
    {:lookup lookup
     :reverse-lookup (c.set/map-invert lookup)
     :remaining []}))

(def rules [rule-6 rule-9 rule-0 rule-3 rule-5 rule-2])

;; end rules

(defn determine-digits
  "Given the signal patterns (which is a list of sets of characters, sorted by size of sets
  increasing) determine the mapping of each set to the digit (0-9) it corresponds to. Return a map
  of char set to digit"
  [signal-patterns]
  ;; Sets of size 2,3,4,7 are known to correspond to digits 1,7,4,8
  (let [known-sets (concat (take 3 signal-patterns) (cons (last signal-patterns) '()))
        lookup (apply assoc {} (interleave known-sets [1 7 4 8]))
        ;; Remaining are sets of size 5 and 6
        unknown-sets (->> signal-patterns (drop 3) (butlast))]
    ;; Given known 1,7,4,8
    ;; 6 is the set of size six which does not contain all of set 7
    ;; 9 is the set of size six which contains all of set 4
    ;; 0 is the remaining set of size six
    ;; Remaining are sets of size 5: 2,3,5
    ;; 3 is the set of size five containing all of set 1
    ;; 5 is the set of size five which set 6 contains all of
    ;; 2 is the remaining set of size five
    (:lookup (reduce (fn [state rule] (rule state)) {:lookup lookup
                                                     :reverse-lookup (c.set/map-invert lookup)
                                                     :remaining unknown-sets} rules))))

(defn unpack
  "Input is a single string needing to be unpacked into multiple strings, then transformed into sets
  of characters"
  [input-str]
  (map (partial into #{}) (s/split input-str #" ")))

;; doesn't work if leading digit is a zero!
;; (def digits->nm "Take seq of single digits, return single number" (comp edn/read-string (partial apply str)))

(defn digits->nm
  "Turn seq of four digits into a single four-digit number"
  [digits]
  (->> digits
       (map vector [1000 100 10 1])
       (map (partial apply *))
       (reduce +)))

(defn solve-input
  "Takes data of one row of input, returns a number"
  [{:keys [signal-patterns output-values] :as data}]
  (let [lookup (determine-digits signal-patterns)]
    (digits->nm (map lookup output-values))))

(defn runner-pt2
  "Each line of input is a separate constraint-based logic puzzle needing to be solved. Parse each
  line into data, solve it (result is a four-digit number), sum up all numbers."
  [input]
  (let [pairs (map #(s/split % #" \| ") input)
        data (map
              (fn [[sps ovs]]
                ;; Data is two lists of sets of chars:
                ;; For first list it's convenient to sort by set size asc
                ;; Second list must stay in given order; corresponds to digits of output number
                {:signal-patterns (sort-by count (unpack sps))
                 :output-values (unpack ovs)})
              pairs)]
    (reduce + (map solve-input data))))

(comment

  ;; pt 2

  (runner-pt2 ["acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"]) ;; 5353

  (runner-pt2 ["be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
               "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
               "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
               "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
               "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
               "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
               "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
               "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
               "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
               "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"]) ;; 61229

  (with-open [r (io/reader (io/resource "aoc-2021/day8.txt"))]
    (runner-pt2 (line-seq r)));; 1073431

  ;; pt 1

  (runner-pt1 ["be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
               "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
               "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
               "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
               "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
               "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
               "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
               "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
               "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
               "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"]) ;; 26

  (with-open [r (io/reader (io/resource "aoc-2021/day8.txt"))]
    (runner-pt1 (line-seq r))) ;; 504

  ;; scratch

  (let [input "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab"]
    (->> input
         unpack
         (sort-by count)
         (determine-digits)))

  ;; {#{\a \b} 1, #{\a \b \d} 7, #{\a \b \e \f} 4, #{\a \b \c \d \e \f \g} 8}

  (digits->nm [3 2 5 4])
  (digits->nm [0 1 9 7])

  )
