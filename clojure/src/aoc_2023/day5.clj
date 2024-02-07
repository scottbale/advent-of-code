(ns aoc-2023.day5
  "If You Give A Seed A Fertilizer."
  ;; formula: Howard's synonym for seed map
  ;; a formula contains many ranges
  ;; a range is three numbers: destination start, source start, range length
  ;; "Is the input value in the range defined by source start and range length?"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as s]
   [clojure.test :refer [deftest are]]
   [debugger :refer [dbg]]))

(defn parse-seed-numbers
  "Parse the first line of input and return a sequence of longs.
  Input is a string that looks like: `seeds: 82 35 23`
  There can be an arbitrary number of integers."
  [input-str]
  (map edn/read-string (s/split (last (s/split input-str #": ")) #" ")))

(defn parse-seed-number-range-pairs
  "Parse the first line of input and return a collection of pairs of longs. Each pair is (a) the start
  of a range, and (b) the count of numbers in the range."
  [input-str]
  (let [nms (parse-seed-numbers input-str)]
    (partition 2 nms)))

(defn parse-range-map
  "A range is a map indicating the destination range start, the source range start, and the length of
  the range. A fourth key, :delta, is a convenience to indicate the difference between destination
  and source range start."
  [range-str]
  (let [[dest-start src-start length] (map edn/read-string (s/split range-str #" "))]
    {:dest-start dest-start
     :src-start src-start
     :length length
     :delta (- dest-start src-start)}))

(defn parse-formula
  "Given a sequence of input strings, return a map representing a formula."
  [input-lines]
  (let [formula-name (first (s/split (first input-lines) #" "))
        range-maps (map parse-range-map (rest input-lines))]
    {:formula formula-name
     :range-maps range-maps}))

(defn parse-formulas
  "Given all of the formula input lines, return an ordered sequence of formulas"
  [input-lines]
  (loop [inputs input-lines results []]
    (if (seq inputs)
      (let [pred (complement s/blank?)
            formula-inputs (take-while pred inputs)
            remaining-inputs (->> inputs (drop-while pred) rest)]
        (recur remaining-inputs (conj results (parse-formula formula-inputs) )))
      results)))

(defn matches-range?
  "Predicate. Is the seed number in the range? The length of the range is the count of numbers in the
  range, and is inclusive of the range start."
  [seed-number {:keys [src-start length]}]
  (<= src-start seed-number (dec (+ src-start length))))

(defn lookup-in-range
  "For a seed number known to be in the range, look up the resulting number."
  [seed-number {:keys [delta]}]
  (+ delta seed-number))

(defn formulated-seed-number
  "Given a seed number and a formula, return the resulting lookup of the seed number in the formula.
  The seed number is either in one of the formula's range maps, determining the result, or the seed
  number is returned unmodified."
  [seed-number {:keys [range-maps]}]
  (if-let [matched-range (first (filter (partial matches-range? seed-number) range-maps))]
    (lookup-in-range seed-number matched-range)
    seed-number))

(defn range-step
  "Takes a seed range and a formula range, returns a collection containing one or two seed ranges. The
  first returned range is the input seed range transformed by the formula range. If a second range
  is returned, it is the remaining portion of the input seed range that falls outside of the formula
  range.
  A seed range is a pair of longs.
  A formula range is a map, one of the :range-maps from a formula."
  [[seed-range-start seed-range-length] {:keys [src-start length delta]}]
  (let [seed-range-start' (+ delta seed-range-start)
        src-end (+ src-start length)
        length' (min seed-range-length (- src-end seed-range-start))]
    (cond-> [[seed-range-start' length']]
      (< length' seed-range-length) (conj [(+ length' seed-range-start) (- seed-range-length length')]))))

(deftest range-step-test
  (are [seed-range formula-range expected-results]
      (let [[ds ss l] formula-range
            formula-map {:dest-start ds :src-start ss :length l :delta (- ds ss)}]
        (= expected-results (range-step seed-range formula-map)))
    [3 6] [22 2 8] [[23 6]]
    [10 5] [20 10 5] [[20 5]]
    [10 5] [19 9 6] [[20 5]]
    [10 5] [20 10 10] [[20 5]]
    [10 5] [19 9 7] [[20 5]]
    [10 5] [20 10 4] [[20 4] [14 1]]
    [10 5] [19 9 5] [[20 4] [14 1]]))

(defn formulated-seed-number-range
  "Given a seed number range and a formula, return the resulting collection of ranges representing the
  entire input range applied to the formula. In other words, the formula may/will produce disjoint
  multiple output ranges given the single input range."
  [seed-range {:keys [range-maps]}]
  (loop [[range-begin :as r] seed-range result-ranges []]
    (if-let [matched-range (first (filter (partial matches-range? range-begin) range-maps))]
      (let [[transformed-range remaining-range] (range-step r matched-range)
            result-ranges (conj result-ranges transformed-range)]
        (if remaining-range
          (recur remaining-range result-ranges)
          result-ranges))
      (conj result-ranges r))))

(defn runner
  "Parse the input into (a) a seq of seed numbers, and (b) seq of formulas. For each seed number,
  chain it through all of the formulas. Return the minimum of the result."
  [input]
  (let [seed-numbers (parse-seed-numbers (first input))
        formulas (parse-formulas (drop 2 input))]
    (letfn [(chain-formulas [formulas seed-number]
              (reduce formulated-seed-number seed-number formulas))]
      (apply min (map (partial chain-formulas formulas) seed-numbers)))))

(defn ranges-contiguous?
  "Given a pair of formula ranges in a vector, are they contiguous? That is, are there any unmapped
  numbers in between the two input ranges?"
  [[r1 r2]]
  (= (+ (:src-start r1) (:length r1)) (:src-start r2)))

(defn non-contiguous-ranges
  "Given a formula, return a (possibly empty) sequence of pairs of formulas which are not contiguous."
  [{:keys [range-maps]}]
  (let [sorted-maps (sort-by :src-start range-maps)
        partitioned-maps (partition 2 1 sorted-maps)]
    (remove ranges-contiguous? partitioned-maps)))

(def formula-contiguous?
  "All ranges in the formula are contiguous?"
  (comp empty? non-contiguous-ranges))

(defn range-pair->repair-range
  "Given a pair of non-contiguous ranges, create and return a new (but inert) range to fill the gap"
  [[{ss1 :src-start
     l1 :length}
    {ss2 :src-start}]]
  (let [src-start (+ ss1 l1)
        length (- ss2 src-start)]
    {:src-start src-start :length length :delta 0 :dest-start src-start}))

(defn repair-formula
  "Given a formula, if the formula has any non-contiguous-ranges, 'repair' it by adding a range to
  cover the gap (but which does no transformation, i.e. src-start and dest-start are identical).
  Otherwise return the unmodified formula."
  [{:keys [formula range-maps] :as f}]
  (if-let [non-contiguous (seq (non-contiguous-ranges f))]
    {:formula formula :range-maps (concat range-maps (map range-pair->repair-range non-contiguous))}
    f))

(defn dbg-runner
  "With this, I learned that my earlier assumption was incorrect. Formulas *can* have ranges that are
  not contiguous."
  [input]
  (let [formulas (->> input (drop 2) parse-formulas (map repair-formula))]
    (every? formula-contiguous? formulas)))

(defn runner2
  "Difference from part 1: the first line of input represents pairs of ranges of numbers (start and
  length). (Attempt #3.)"
  [input]
  (let [seed-number-ranges (parse-seed-number-range-pairs (first input))
        formulas (->> input (drop 2) parse-formulas (map repair-formula))
        mapped-seed-number-ranges (reduce (fn [seed-ranges formula]
                                            (mapcat #(formulated-seed-number-range % formula) seed-ranges)) seed-number-ranges formulas)]
    (reduce min (map first mapped-seed-number-ranges))))

(comment

  (runner ["seeds: 79 14 55 13"
           ""
           "seed-to-soil map:"
           "50 98 2"
           "52 50 48"
           ""
           "soil-to-fertilizer map:"
           "0 15 37"
           "37 52 2"
           "39 0 15"
           ""
           "fertilizer-to-water map:"
           "49 53 8"
           "0 11 42"
           "42 0 7"
           "57 7 4"
           ""
           "water-to-light map:"
           "88 18 7"
           "18 25 70"
           ""
           "light-to-temperature map:"
           "45 77 23"
           "81 45 19"
           "68 64 13"
           ""
           "temperature-to-humidity map:"
           "0 69 1"
           "1 0 69"
           ""
           "humidity-to-location map:"
           "60 56 37"
           "56 93 4"]) ;; 35

  (runner2 ["seeds: 79 14 55 13"
            ""
            "seed-to-soil map:"
            "50 98 2"
            "52 50 48"
            ""
            "soil-to-fertilizer map:"
            "0 15 37"
            "37 52 2"
            "39 0 15"
            ""
            "fertilizer-to-water map:"
            "49 53 8"
            "0 11 42"
            "42 0 7"
            "57 7 4"
            ""
            "water-to-light map:"
            "88 18 7"
            "18 25 70"
            ""
            "light-to-temperature map:"
            "45 77 23"
            "81 45 19"
            "68 64 13"
            ""
            "temperature-to-humidity map:"
            "0 69 1"
            "1 0 69"
            ""
            "humidity-to-location map:"
            "60 56 37"
            "56 93 4"]) ;; 46

  (with-open [r (io/reader (io/resource "aoc-2023/day5.txt"))]
    (runner (line-seq r))) ;; 993500720

  (with-open [r (io/reader (io/resource "aoc-2023/day5.txt"))]
    (runner2 (line-seq r)))
  ;; 4917124 after 'repairing' formula range-maps
  ;; 60756547 too high "Elapsed time: 7.461912 msecs"

  (parse-seed-numbers "seeds: 2 3 5")
  (parse-seed-numbers "seeds: 2 3 5 83")

  (parse-seed-number-range-pairs "seeds: 79 14 35 15") ;; ((79 14) (35 15))
  (parse-seed-number-range-pairs "seeds: 79 14 35 15 22 8") ;; ((79 14) (35 15) (22 8))

  (parse-range-map "0 15 37")

  (parse-formula 
   ["soil-to-fertilizer map:"
    "0 15 37"
    "37 52 2"
    "39 0 15"])

  (parse-formulas
   ["seed-to-soil map:"
    "50 98 2"
    "52 50 48"
    ""
    "soil-to-fertilizer map:"
    "0 15 37"
    "37 52 2"
    "39 0 15"
    ""
    "fertilizer-to-water map:"
    "49 53 8"
    "0 11 42"
    "42 0 7"
    "57 7 4"])

  (matches-range? 2 {:dest-start 49 :src-start 53 :length 8}) ;; false
  (matches-range? 52 {:dest-start 49 :src-start 53 :length 8}) ;; false
  (matches-range? 53 {:dest-start 49 :src-start 53 :length 8}) ;; true
  (matches-range? 54 {:dest-start 49 :src-start 53 :length 8}) ;; true
  (matches-range? 60 {:dest-start 49 :src-start 53 :length 8}) ;; true
  (matches-range? 61 {:dest-start 49 :src-start 53 :length 8}) ;; false
  (matches-range? 62 {:dest-start 49 :src-start 53 :length 8}) ;; false

  (lookup-in-range 60 {:dest-start 49 :src-start 53 :length 8 :delta -4}) ;;56

  (formulated-seed-number
   3
   {:formula "fertilizer-to-water"
    :range-maps
    '({:dest-start 49 :src-start 53 :length 8 :delta -4}
      {:dest-start 0 :src-start 11 :length 42 :delta -11}
      {:dest-start 42 :src-start 0 :length 7 :delta 42}
      {:dest-start 57 :src-start 7 :length 4 :delta 50})}) ;; 45

  (formulated-seed-number-range [2 1] {:range-maps []}) ;; [[2 1]]
  (formulated-seed-number-range [2 5] {:range-maps [{:dest-start 49 :src-start 2 :length 8 :delta 47}]}) ;; [[49 5]]
  (formulated-seed-number-range [2 8] {:range-maps [{:dest-start 49 :src-start 2 :length 5 :delta 47}]}) ;; [[49 5] [7 3]]
  (formulated-seed-number-range [2 8] {:range-maps [{:dest-start 49 :src-start 2 :length 8 :delta 47}]}) ;; [[49 8]]
  (formulated-seed-number-range [2 8] {:range-maps [{:dest-start 49 :src-start 2 :length 7 :delta 47}]}) ;; [[49 7] [9 1]]
  (formulated-seed-number-range [2 8] {:range-maps [{:dest-start 49 :src-start 2 :length 5 :delta 47}
                                                    {:dest-start 12 :src-start 7 :length 3 :delta 5}]}) ;; [[49 5] [12 3]]
  (formulated-seed-number-range [2 8] {:range-maps [{:dest-start 48 :src-start 1 :length 6 :delta 47}
                                                    {:dest-start 12 :src-start 7 :length 3 :delta 5}]}) ;; [[49 5] [12 3]]
  (formulated-seed-number-range [2 9] {:range-maps [{:dest-start 48 :src-start 1 :length 4 :delta 47}
                                                    {:dest-start 12 :src-start 5 :length 3 :delta 7}
                                                    {:dest-start 1  :src-start 8 :length 8 :delta -7}]}) ;; [[49 3][12 3][1 3]]

  ;; This is invalid input.
  ;; It's not obvious from the puzzle description, but just eyeballing the data, it appears the range maps never have disjoint
  ;; ranges (ranges with unmapped numbers in between). If so, this simplifies the work because once an unmapped number is found
  ;; I can assume the rest of the seed number range is unmapped.
  ;; [later] Not true, it turns out! See 'repair ranges'-related functions
  (formulated-seed-number-range [2 8] {:range-maps [{:dest-start 49 :src-start 2 :length 3 :delta 47}
                                                    {:dest-start 12 :src-start 7 :length 3 :delta 5}]}) ;; expect [[49 3] [5 2] [7 3]]

  (formulated-seed-number-range
   [2 1]
   {:range-maps
    '({:dest-start 49 :src-start 53 :length 8 :delta -4}
      {:dest-start 0 :src-start 11 :length 42 :delta -11}
      {:dest-start 42 :src-start 0 :length 7 :delta 42}
      {:dest-start 57 :src-start 7 :length 4 :delta 50})}) ;; [[44 1]]

  (with-open [r (io/reader (io/resource "aoc-2023/day5.txt"))]
    (dbg-runner (line-seq r)))

  (let [f {:formula "foo"
           :range-maps
           [{:dest-start 31 :src-start 21 :length 10 :delta 10}
            {:dest-start 51 :src-start 41 :length 10 :delta 10}
            {:dest-start 11 :src-start 1 :length 10 :delta 10}
            {:dest-start 21 :src-start 11 :length 10 :delta 10}
            {:dest-start 41 :src-start 32 :length 9 :delta 9}]}]
    ;;   (formula-contiguous? f)
    ;;   (non-contiguous-ranges f)
    (repair-formula f))

  (range-pair->repair-range
   '({:dest-start 31 :src-start 21 :length 10 :delta 10}
     {:dest-start 41 :src-start 32 :length 9 :delta 9}))
  ;; {:src-start 31 :length 1 :delta 0 :dest-start 31}


  ;; old stuff below this line

  (partition 2 1 [1 3 5 7])                  ;; ((1 3) (3 5) (5 7))
  (partition 2 1 [Long/MAX_VALUE] [1 3 5 7]) ;; ((1 3) (3 5) (5 7) (7 9223372036854775807))

  (defn binary-search
    "Search "
    [val sorted-input]
    ;; val == input value
    ;; sorted-input == sorted seq of range starts
    (let [comparator (fn [v [a b]]
                       (cond 
                         (< v a)  1  ;; too high, search lower in the input
                         (<= b v) -1 ;; too low, search higher in the input
                         ;; it's a match when a <= v and v < b
                         :else 0))]
      (loop [vals (partition 2 1 [Long/MAX_VALUE] sorted-input)
             safety 20]
        (if (and (seq vals) (< 0 safety))
          (let [n (count vals)
                half-n (long (/ n 2))
                check (nth vals half-n)
                i (comparator val check)]
            (case i
              -1 (recur (drop half-n vals) (dec safety))
              1 (recur (take half-n vals) (dec safety))
              0 (first check)))
          :not-good))
      )
    )

  ;; taken from "fertilizer-to-water" example
  (binary-search 45 [0 42 49 57])  ;; 42
  (binary-search 45 [0 42 43 44])  ;; 44
  (binary-search 45 [45 47 58 69]) ;; 45
  (binary-search 45 [46 47 58 69]) ;; :not-good

  (binary-search 45 [45 47 58 69]) ;; 45

  (->>
   [2659452899 3773423191 23529065
    1010417677 1830019321 229964714
    1506263997 1764304095 65715226
    3017023682 3993999178 103632805
    3758361154 3931294907 62704271
    2513441862 2529586713 106552791
    3821065425 3163657189 7959671
    3410504451 3191697730 271334719
    2500616406 3150831733 12825456
    2065874786 2636139504 257698620
    4142272690 2382216135 108163002
    1377732678 1378901025 61208694
    91217027 248578952 8927711
    2463617376 3879075083 36999030
    3982807123 2315058258 67157877
    2323573406 2065874786 97274446
    958870382 916323074 51547295
    3868386197 3579887474 114420926
    931392999 1351423642 27477383
    2942753127 3694308400 74270555
    1812734437 168620508 79958444
    3301364949 2163149232 3197696
    2420847852 2166346928 42769524
    3829025096 3111470632 39361101
    2619994653 2490379137 39207576
    1571979223 1523548881 240755214
    2927532333 3916074113 15220794
    3125500723 4097631983 175864226
    1438941372 10080856 67322625
    2049903179 0 10080856
    3304562645 2209116452 105941806
    1976132043 1277652506 73771136
    2659202229 3171616860 250670
    4256036535 3463032449 38930761
    1240382391 257506663 137350287
    0 77403481 91217027
    3120656487 3768578955 4844236
    100144738 967870369 309782137
    409926875 394856950 521466124
    2682981964 4273496209 21471087
    2704453051 3501963210 77924264
    2802207515 2893838124 125324818
    3681839170 3796952256 76521984
    4250435692 3873474240 5600843
    1892692881 1440109719 83439162
    4049965000 3019162942 92307690
    2782377315 3171867530 19830200]
   (partition 3)
   (map first)
   sort)

  (time (binary-search 211169946 '(0 91217027 100144738 409926875 931392999 958870382 1010417677 1240382391 1377732678 1438941372 1506263997 1571979223 1812734437 1892692881 1976132043 2049903179 2065874786 2323573406 2420847852 2463617376 2500616406 2513441862 2619994653 2659202229 2659452899 2682981964 2704453051 2782377315 2802207515 2927532333 2942753127 3017023682 3120656487 3125500723 3301364949 3304562645 3410504451 3681839170 3758361154 3821065425 3829025096 3868386197 3982807123 4049965000 4142272690 4250435692 4256036535))) ;; 100144738



  ;; end comment
  )
