(ns aoc-2021.day1
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [debugger :refer [dbg]]))


(defn runner
  "Run the puzzle with sequence of puzzle input, return the answer"
  [input]
  (let [diffs (map - input (cons 0 input))]
    (->> diffs 
         rest
         (filter pos?)
         count)))

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
