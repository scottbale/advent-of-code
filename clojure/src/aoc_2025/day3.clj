(ns aoc-2025.day3
  "Max battery bank joltage."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]))

(defn nm-str->digits-seq
  [nm-str]
  (mapv (comp edn/read-string str) nm-str))

(defn max-n-digits
  "Given a sequence of 1-digit numbers, find the largest `n`-digit number that can
  be assembled from in-order input digits.

  Recursive solution to build a result one digit at a time, in decreasing order of
  magnitude:
  * Find the max among the `search-digits` (but don't search the trailing (n-1)
    `excluded-digits`, reserved for the next digit(s) search)
  * shrink `excluded-digits` (termination condition)
  * Accumulate `result-digits`
  * At the next recur: only search the `search-digits` after the last max _but_
    append the next previously excluded digit"
  [n digits]
  (->>
   (loop [search-digits (drop-last (dec n) digits)
          excluded-digits (take-last (dec n) digits)
          result-digits []]
     (let [mx (apply max search-digits)
           result-digits (conj result-digits mx)]
       (if (seq excluded-digits)
         (recur
          (conj (->> search-digits (drop-while #(not= % mx)) rest vec) (first excluded-digits))
          (rest excluded-digits)
          result-digits)
         result-digits)))
   (apply str)
   edn/read-string))

(defn runner
  "runner docstring"
  [input]
  (->> input
       (map (comp (partial max-n-digits 2) nm-str->digits-seq))
       (reduce + 0)))

(defn runner2
  "runner docstring"
  [input]
  (->> input
       (map (comp (partial max-n-digits 12) nm-str->digits-seq))
       (reduce + 0)))


(comment

  (nm-str->digits-seq "1122334")

  (max-n-digits 2 (nm-str->digits-seq "1122334"))
  (max-n-digits 2 (nm-str->digits-seq "8675903"))
  (max-n-digits 2 (nm-str->digits-seq "987654321111111"))
  (max-n-digits 3 (nm-str->digits-seq "987654321111111"))
  (max-n-digits 2 (nm-str->digits-seq "234234234234278"))
  (max-n-digits 5 (nm-str->digits-seq "234234234234278"))

  (runner ["987654321111111"
           "811111111111119"
           "234234234234278"
           "818181911112111"]) ;; 357

  (runner2 ["987654321111111"
           "811111111111119"
           "234234234234278"
           "818181911112111"]) ;; 3121910778619

  (with-open [r (io/reader (io/resource "aoc-2025/day3.txt"))]
    (runner (line-seq r))) ;;17281

  (with-open [r (io/reader (io/resource "aoc-2025/day3.txt"))]
    (runner2 (line-seq r))) ;; 171388730430281

  ;; end comment
  )
