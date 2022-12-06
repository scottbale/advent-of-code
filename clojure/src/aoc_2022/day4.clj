(ns aoc-2022.day4
  "camp cleanup - overlapping sections"
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn parse-range
  "Parse a range string like '23-45' into a range of longs"
  [r]
  (let [[i j] (str/split r #"-")]
    (range (Long/parseLong i) (inc (Long/parseLong j)))))

(defn range->bigint
  "Construct a binary representation of the given range, a BigInteger in which all the bits of the range are
  flipped to one and all other bits are zero."
  [r]
  (letfn [(set-a-bit [val i]
            (.setBit val i))]
    (reduce set-a-bit (biginteger 0) r)))

(defn parse-input
  "parse a line of input like '23-46,5-71', returning two ranges of numbers"
  [input]
  (let [ranges (str/split input #",")]
    (map (comp range->bigint parse-range) ranges)))

(defn fully-overlapping?
  "Given a pair of two sections (represented by two BigIntegers), they are fully overlapping if doing a
  bitwise OR produces one or the other BigInteger."
  [[biginteger1 biginteger2 :as bigintegers]]
  (not (nil? ((set bigintegers) (.or biginteger1 biginteger2)))))

(defn overlapping?
  "Given a pair of two sections (represented by two BigIntegers), they are overlapping if doing a
  bitwise AND is greater than zero."
  [[biginteger1 biginteger2]]
  (< 0 (.and biginteger1 biginteger2)))

(defn runner1
  "Each line of input is two ranges, count how many range pairs have one fully containing the other."
  [input]
  (->> input
       (map parse-input)
       (filter fully-overlapping?)
       count))

(defn runner
  "Each line of input is two ranges, count how many range pairs have one overlapping the other."
  [input]
  (->> input
       (map parse-input)
       (filter overlapping?)
       count))


(comment

  (runner ["2-4,6-8"
           "2-3,4-5"
           "5-7,7-9"
           "2-8,3-7"
           "6-6,4-6"
           "2-6,4-8"]) ;; 2

  (with-open [r (io/reader (io/resource "aoc-2022/day4.txt"))]
    (runner (line-seq r))) ;; 843 ;; 547

  (parse-input "2-4,6-8")
  (parse-input "5-7,7-9")
  (parse-range "2-4")
  (range->bigint (range 1 3))
  (fully-overlapping? [(biginteger 1) (biginteger 3)])
  (fully-overlapping? [(biginteger 3) (biginteger 3)])
  (fully-overlapping? [(biginteger 3) (biginteger 14)])
  (overlapping? [(biginteger 3) (biginteger 3)])
  (overlapping? [(biginteger 3) (biginteger 14)])
  (overlapping? [(biginteger 1) (biginteger 4)])
  (overlapping? [(biginteger 224) (biginteger 896)])

  )
