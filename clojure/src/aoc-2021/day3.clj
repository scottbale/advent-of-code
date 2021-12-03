(ns aoc-2021.day3
  "Namespace docstring - don't forget to delete unused requires"
  (:require
   [aoc-2020.day2 :refer [nm]]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [debugger :refer [dbg]]
   [the.parsatron :as p]))

(defn read-binary
  "Read one of the e.g. '10010' input values as a binary literal (a long)"
  [input-str]
  (edn/read-string (str "2r" input-str)))

(defn common-bit
  "Average the incoming numbers (all zeroes or ones), Return zero or one depending on whether the
  average is closer to zero or one."
  [numbers]
  (let [n (count numbers)
        n-half (/ n 2)
        sum (reduce + numbers)]
    (cond
      (< sum n-half) 0
      (> sum n-half) 1
      :error (RuntimeException. "Neither bit is more frequent"))))

(defn common-bit-at-bit
  "For the input seq and the bit-nm index, calculate the common bit and bitshift the result"
  [input [bit-mask shift-times]]
  (let [map-fn (fn [an-input] 
                 (bit-shift-right (bit-and bit-mask an-input) shift-times))
        result-bit (->> input
                        (map map-fn)
                        common-bit)]
    (bit-shift-left result-bit shift-times)))

(def bit-thingies "Precompute pairs of [2^n (dec n)] for n equals 1 to 12"
  [[1 0] [2 1] [4 2] [8 3] [16 4] [32 5]
   [64 6] [128 7] [256 8] [512 9] [1024 10] [2048 11]])

(defn runner
  "Runner docstring"
  [input]
  (let [n (count input)
        width (count (first input))
        codes (map read-binary input)
        gamma (reduce + (map (partial common-bit-at-bit codes) (take width bit-thingies)))
        xor-mask (edn/read-string (apply str "2r" (repeat width "1")))
        epsilon (bit-xor gamma xor-mask)]
    (* gamma epsilon)))

(comment

  (common-bit [1 0 1 0 0])
  (common-bit [1 0 1 0 1 1])
  (common-bit-at-bit [1 0 1 0 1 1] [1 0])
  (common-bit-at-bit [2 0 3 0 2 2] [2 1])
  (common-bit-at-bit [2 1 3 1 3 3] [2 1])
  (common-bit-at-bit [16 1 3 1 3 3] [16 4])
  (common-bit-at-bit [16 0 0] [16 4])
  (common-bit-at-bit [16 15 15] [16 4])
  (common-bit-at-bit [16 8 8 8 8 8] [16 4])
  ;; wtf
  (common-bit-at-bit '(4 30 22 23 21 15 7 28 16 25 2 10) [8 3])
  (map (partial bit-and 8) '(4 30 22 23 21 15 7 28 16 25 2 10))
  (common-bit '(0 8 0 0 0 8 0 8 0 8 0 8))

  ;; Run with test input from the puzzle description
  (runner ["00100"
           "11110"
           "10110"
           "10111"
           "10101"
           "01111"
           "00111"
           "11100"
           "10000"
           "11001"
           "00010"
           "01010"])

  ;; 1st three of actual input
  (runner ["111011001010"
           "010011101110"
           "110001001010"])

  (common-bit [1 0 1 0 1 1])
  (common-bit-at-bit [1 0 1 0 1 1] [1 0])


  ;; Run with the real input, get the final answer
  (with-open [r (io/reader (io/resource "aoc-2021/day3.txt"))]
    (runner (line-seq r))) ;; 1025636


  )
