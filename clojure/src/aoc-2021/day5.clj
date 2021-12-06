(ns aoc-2021.day5
  "Overlapping thermal vent lines"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as s]
   [debugger :refer [dbg]]))

(defn horizontal?
  "predicate - true IFF line is horizontal"
  [[[x1 y1] [x2 y2]]]
  (== x1 x2))

(defn vertical?
  "predicate - true IFF line is vertical"
  [[[x1 y1] [x2 y2]]]
  (== y1 y2))

(defn ranger
  "Return a range of both points, inclusive, taking into account direction"
  [x1 x2]
  (let [step (if (< x1 x2) 1 -1)]
    (range x1 (+ x2 step) step)))

(defn line->points
  "Given any input line, return the seq of all points of that line, inclusive"
  [[[x1 y1] [x2 y2] :as line]]
  (let [xs (if (horizontal? line) (repeat x1) (ranger x1 x2))
        ys (if (vertical? line) (repeat y1) (ranger y1 y2))]
    (map vector xs ys)))

(defn count-point
  "Map a point to a count of that point"
  [counts point]
  (update counts point (fnil inc 0)))

(defn parse-line
  "Parse string like '0,9 -> 5,9' into vector of two pairs [[0 9] [5 9]]"
  [line-string]
  (let [result (s/split line-string #" -> ")
        point-parser (fn [point-string]
                       (map edn/read-string (s/split point-string #",")))]
    (map point-parser result)))

(defn runner-pt1
  "Consider only input lines that are horizontal or vertical. Count the number of points that overlap
  by two or more."
  [input]
  (->> input
       (map parse-line)
       (filter (some-fn horizontal? vertical?))
       (mapcat line->points)
       (reduce count-point {})
       (filter (fn [[k v]] (>= v 2)))
       count))

(defn runner-pt2
  "Like pt 1, but also consider the remaining input lines which include diagonal."
  [input]
  (->> input
       (map parse-line)
       (mapcat line->points)
       (reduce count-point {})
       (filter (fn [[k v]] (>= v 2)))
       count))


(comment

  (runner-pt2 ["0,9 -> 5,9"
               "8,0 -> 0,8"
               "9,4 -> 3,4"
               "2,2 -> 2,1"
               "7,0 -> 7,4"
               "6,4 -> 2,0"
               "0,9 -> 2,9"
               "3,4 -> 1,4"
               "0,0 -> 8,8"
               "5,5 -> 8,2"])

  (with-open [r (io/reader (io/resource "aoc-2021/day5.txt"))]
    (runner-pt2 (line-seq r))) ; 21698

  (runner-pt1 ["0,9 -> 5,9"
               "8,0 -> 0,8"
               "9,4 -> 3,4"
               "2,2 -> 2,1"
               "7,0 -> 7,4"
               "6,4 -> 2,0"
               "0,9 -> 2,9"
               "3,4 -> 1,4"
               "0,0 -> 8,8"
               "5,5 -> 8,2"])

  (with-open [r (io/reader (io/resource "aoc-2021/day5.txt"))]
    (runner-pt1 (line-seq r))) ;; 5294

  ((some-fn horizontal? vertical?) [[1 1] [2 2]])
  ((some-fn horizontal? vertical?) [[1 1] [2 1]])
  ((some-fn horizontal? vertical?) [[1 1] [1 2]])

  ;; nil-safe counting
  (update {[1 1] 1} [1 1] (fnil inc 0))

  (parse-line "0,9 -> 5,9")

  ;; filter map entries based on value
  (filter 
   (fn [[k v]] (>= v 2))
   {[1 1] 1 [8 3] 2}
   )

  (ranger 1 4)
  (ranger 4 1)

  (line->points [[1 1] [1 4]])
  (line->points [[1 1] [4 1]])
  (line->points [[8 2] [4 2]])
  (line->points [[1 1] [4 4]])
  (line->points [[4 1] [1 4]])

  ;; does this truncate the longer input? Yes
  (map vector [1 2] [:a :b :c])

  ;; Cool Clojure things I was reminded of:
  ;; some-fn
  ;; fnil

)
