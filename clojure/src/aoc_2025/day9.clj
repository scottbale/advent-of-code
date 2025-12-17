(ns aoc-2025.day9
  "Red/green tiles"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as s]
   [debugger :refer [dbg]]))

(defn parse-point
  [input]
  (mapv edn/read-string (s/split input #",")))

(defn area
  [[[x1 y1] [x2 y2]]]
  (* (inc (abs (- x2 x1))) (inc (abs (- y2 y1)))))

(defn runner
  "runner docstring"
  [input]
  (let [points-alist
        (->> input
             (mapv parse-point)
             (mapv vector (range)))
        point-pairs
        (for [[i p1] points-alist
              [j p2] points-alist
              :when (< i j)]
          [p1 p2])]
    (->> point-pairs
         (mapv area)
         sort
         last)))


(comment

  (parse-point "7,1")
  (area [[7 1] [11 1]])
  (area [[11 1] [7 1]])
  (area [[7 3] [2 3]])
  (area [[2 3] [7 3]])
  (area [[2 5] [11 1]])
  (area [[11 1] [2 5]])

  (runner ["7,1"
           "11,1"
           "11,7"
           "9,7"
           "9,5"
           "2,5"
           "2,3"
           "7,3"]) ;; 50

  (with-open [r (io/reader (io/resource "aoc-2025/day9.txt"))]
    (runner (line-seq r)));; 4750092396

  ;; end comment
  )
