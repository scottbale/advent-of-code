(ns aoc-2021.day1
  "Numbers represent ocean floor depths; count the number of increasing adjacent depths. Part two, use
  sliding window of three adjacent depth sums, calculate diffs of those.
  https://adventofcode.com/2021/day/1"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [debugger :refer [dbg]]))


(defn runner-pt1
  "Input is sequence of integers repreresenting ocean floor depths. Transform into a sequence of diffs
  of contiguous depths, and count the number of increases."
  [input]
  (let [diffs (map - input (cons 0 input))]
    (->> diffs 
         rest ;; drop first element b/c it's meaningless
         (filter pos?)
         count)))

(defn runner
  "Input is sequence of integers repreresenting ocean floor depths. Calculate sequence of sliding
  windows of three contiguous depths. Sum each sliding window, diff each adjacent sliding window,
  count number of increases."
  [input]
  (let [sums (->> input
                  (partition 3 1) ;; sliding windows
                  (map (partial apply +))) ;; sums of each window
        diffs (rest (map - sums (cons 0 sums)))]
    (->> diffs (filter pos?) count)))

(comment

  ;; Run with test input from the puzzle description
  (runner [199
           200
           208
           210
           200
           207
           240
           269
           260
           263])

  ;; Run with the real input, get the final answer
  (with-open [r (io/reader (io/resource "aoc-2021/day1.txt"))]
    (runner (map edn/read-string (line-seq r))))


  )
