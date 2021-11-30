(ns aoc-2021.day1
  (:require
   [clojure.java.io :as io]
   [debugger :refer [dbg]]))


(defn runner
  "Run the puzzle with sequence of puzzle input, return the answer"
  [input]
  )

(comment

  ;; Run with test input from the puzzle description
  (runner ["puzzle test input line 1"
           "puzzle test input line 2"])

  ;; Run with the real input, get the final answer
  (with-open [r (io/reader (io/resource "aoc-2021/day1.txt"))]
    (runner (line-seq r)))


  )
