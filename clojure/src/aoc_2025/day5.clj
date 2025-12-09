(ns aoc-2025.day5
  "Spoiled ingredients."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as s]))

(defn parse-range
  "Parse a range string like '23-26' into a vector pair of longs."
  [r]
  (let [[i j] (s/split r #"-")]
    [(edn/read-string i) (edn/read-string j)]))

(defn in-range?
  [[i j] n]
  (<= i n j))

(defn runner
  "Return the count of fresh ingredient ids (numbers (the 2nd half of the input)
  which are in any of the ranges (the first half of the input))."
  [input]
  (let [ranges (take-while (fn [l] (-> l s/blank? not)) input)
        range-pairs (map parse-range ranges)
        nums (->>
              input
              (drop (-> ranges count inc))
              (map edn/read-string))]
    (->> nums
         (filter (fn [n]
                   (some
                    (fn [range-pair]
                      (in-range? range-pair n))
                    range-pairs))) ;; seq of fresh nums
         count)))

(defn condense-ranges
  "Given a sequence of range pairs sorted by the first number, return a sequence
  of fewer ranges, with overlapping ranges condensed into single ranges."
  [[r1 r2 :as ranges]]
  (loop [result-ranges []
         [i1 i2 :as r-current] r1
         [j1 j2 :as r-next] r2
         remaining-ranges (drop 2 ranges)]
    (if r-next
      (if (<= j1 (inc i2))
        ;; combine
        (recur result-ranges [i1 (max i2 j2)] (first remaining-ranges) (rest remaining-ranges))
        ;; don't combine
        (recur (conj result-ranges r-current) r-next (first remaining-ranges) (rest remaining-ranges)))
      (conj result-ranges r-current))))

(defn runner2
  "Ignoring the 2nd half of the input, combine all of the overlapping ranges of
  the input's first half and count the total number of numbers."
  [input]
  (let [ranges (->>
                input
                (take-while (fn [l] (-> l s/blank? not)))
                (map parse-range))]
    (->>
     ranges
     (sort-by first)
     (condense-ranges)
     (map (fn [[r1 r2]] (inc (- r2 r1))))
     (reduce + 0))))

(comment

  (condense-ranges '([3 5] [10 14] [12 18] [16 20])) ;; [[3 5] [10 20]]
  (condense-ranges '([3 5] [10 14] [12 20] [16 18])) ;; [[3 5] [10 20]]

  (runner2 ["3-5"
            "10-14"
            "16-20"
            "12-18"
            ""
            "1"
            "5"
            "8"
            "11"
            "17"
            "32"]) ;; 14

  (- 274218950644343 274123287977170) ;; 95,662,667,173

  (in-range? [3 5] 4)
  (in-range? [3 5] 3)
  (in-range? [3 5] 5)

  (runner ["3-5"
           "10-14"
           "16-20"
           "12-18"
           ""
           "1"
           "5"
           "8"
           "11"
           "17"
           "32"]) ;; 3

  (with-open [r (io/reader (io/resource "aoc-2025/day5.txt"))]
    (runner (line-seq r))) ;; 598

  (with-open [r (io/reader (io/resource "aoc-2025/day5.txt"))]
    (runner2 (line-seq r))) ;; 360341832208407
  ;; 333404221810494 too low



  ;; end comment
  )
