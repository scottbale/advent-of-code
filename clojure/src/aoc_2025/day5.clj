(ns aoc-2025.day5
  "Spoiled ingredients."
  (:require
   [aoc-2022.day4 :as aoc22d4]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as s]
   [debugger :refer [dbg]]))

(defn range-str->big-int
  "Given a range string like '3-5', return a BigInteger representation of that
  range of numbers. It is a binary representation of the range, in which all of
  the bits of the range are flipped to one."
  [range-str]
  (-> range-str aoc22d4/parse-range aoc22d4/range->bigint))

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


(comment

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

  ;; end comment
  )
