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
  be assembled from in-order input digits."
  [n digits]
  (->>
   (loop [search-digits (drop-last (dec n) digits)
          next-digits (take-last (dec n) digits)
          result-digits []
          i 0]
     ;; (println "---------------------------iteration" i)
     (let [mx (apply max search-digits)
           result-digits (conj result-digits mx)]
       ;; (println "search digits:" search-digits)
       ;; (println "next digits:" next-digits)
       ;; (println "max:" mx)
       ;; (println "results:" result-digits)
       (if (seq next-digits)
         (recur
          (conj (->> search-digits (drop-while #(not= % mx)) rest vec) (first next-digits))
          (rest next-digits)
          result-digits
          (inc i))
         result-digits)))
   (apply str)
   edn/read-string))

(defn runner
  "runner docstring"
  [input]
  (->> input
       (map (comp (partial max-n-digits 2) nm-str->digits-seq))
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
           "818181911112111"])

  (with-open [r (io/reader (io/resource "aoc-2025/day3.txt"))]
    (runner (line-seq r))) ;;17281

  ;; end comment
  )
