(ns aoc-2021.day3
  "Submarine status codes encoded in binary input"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [debugger :refer [dbg]]))

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
      (>= sum n-half) 1)))

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

(defn runner-pt1
  "Runner docstring"
  [input]
  (let [n (count input)
        width (count (first input))
        codes (map read-binary input)
        gamma (reduce + (map (partial common-bit-at-bit codes) (take width bit-thingies)))
        xor-mask (edn/read-string (apply str "2r" (repeat width "1")))
        epsilon (bit-xor gamma xor-mask)]
    (* gamma epsilon)))

(defn sonic-recursor 
  "Do the thing with teh stuff"
  [in [[_ bit-position :as bit-thing] & more]]
  (let [the-bit (common-bit-at-bit in bit-thing)
        filter-fn (fn [i] (bit-test i bit-position))
        in' (if (pos? the-bit)
              (remove filter-fn in)
              (filter filter-fn in))]
    (if (== 1 (count in'))
      (first in')
      (recur in' more))))

(defn runner
  "Runner docstring"
  [input]
  (let [n (count input)
        width (count (first input))
        codes (map read-binary input)
        ;;
        o2 (sonic-recursor codes (reverse (take width bit-thingies)))
        ;;
        xor-mask (edn/read-string (apply str "2r" (repeat width "1")))
        co2 (bit-xor o2 xor-mask)]
    (dbg [o2 co2])
    ;;(* o2 co2)
    o2
    ))

(comment

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

  ;; Run with the real input, get the final answer
  (with-open [r (io/reader (io/resource "aoc-2021/day3.txt"))]
    (runner (line-seq r))) ;; pt 2 c02 257
  ;; pt 2 o2 [3089 1006]        ;; pt 1 1025636
  (* 3089 257) ;; 793873

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


  (common-bit [1 0 1 0 1 1])
  (common-bit-at-bit [1 0 1 0 1 1] [1 0])




  )
