(ns aoc-2022.day1
  "Elf calories carried"
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [debugger :refer [dbg]]))

(defn total
  [strs]
  (->> strs
       (map #(Integer/parseInt %))
       (reduce +)))

(defn runner
  "Each group of numbers (groups separated by blank line) are calories carried by elves. What is the
  largest total?"
  [input]
  (->> input
       (partition-by str/blank?)
       (remove (comp str/blank? first))
       (map total)
       (sort >)
       first))

(defn runner2
  "Each group of numbers (groups separated by blank line) are calories carried by elves. Find the three largest totals, and total them."
  [input]
  (->> input
       (partition-by str/blank?)
       (remove (comp str/blank? first))
       (map total)
       (sort >)
       (take 3)
       (reduce +)))


(comment

  (runner2 ["1000"
            "2000"
            "3000"
            ""
            "4000"
            ""
            "5000"
            "6000"
            ""
            "7000"
            "8000"
            "9000"
            ""
            "10000"]) ;; 45000

  (with-open [r (io/reader (io/resource "aoc-2022/day1.txt"))]
    (runner2 (line-seq r)));;203420 ;; 68467


  )
