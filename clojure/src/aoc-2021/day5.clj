(ns aoc-2021.day5
  "Thermal vent lines"
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

(defn line->points
  "Given a horizontal or vertical line, return the seq of all points"
  [[[x1 y1] [x2 y2] :as point]]
  (let [x-min (min x1 x2)
        x-max (max x1 x2)
        y-min (min y1 y2)
        y-max (max y1 y2)]
    (for [x (range x-min (inc x-max))
          y (range y-min (inc y-max))]
      [x y])))

(defn count-point
  "Map a point to a count of that point"
  [counts point]
  (update counts point (fnil inc 0)))

(defn parse-point
  "Parse string like '0,9 -> 5,9' into vector of two pairs [[0 9] [5 9]]"
  [line-string]
  (let [result (s/split line-string #" -> ")
        point-parser (fn [point-string]
                       (map edn/read-string (s/split point-string #",")))]
    (map point-parser result)))

(defn runner-pt1
  [input]
  (->> input
       (map parse-point)
       (filter (some-fn horizontal? vertical?))
       (mapcat line->points)
       (reduce count-point {})
       (filter (fn [[k v]] (>= v 2)))
       count
       ))


(comment

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

  (line->points [[1 1] [1 4]])
  (line->points [[1 1] [4 1]])
  (line->points [[8 2] [4 2]])

  (update {[1 1] 1} [1 1] (fnil inc 0))

  (parse-point "0,9 -> 5,9")

  (filter 
   (fn [[k v]] (>= v 2))
   {[1 1] 1 [8 3] 2}
   )
)
