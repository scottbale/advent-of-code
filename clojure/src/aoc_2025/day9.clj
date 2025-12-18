(ns aoc-2025.day9
  "Red/green tiles"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set]
   [clojure.string :as s]))
;; todo lib.grid

(defn parse-point
  [input]
  (mapv edn/read-string (s/split input #",")))

(defn area
  "Given a pair of two points which are opposite corners of a rectangle, return
  the numeric area of the rectangle"
  [[[x1 y1] [x2 y2]]]
  (* (inc (abs (- x2 x1))) (inc (abs (- y2 y1)))))

(defn area-points
  "Given a pair of two points which are opposite corners of a rectangle, return a
  collection of all points of that rectangle."
  [[[x1 y1] [x2 y2]]]
  (let [[x-min x-max] (if (<= x1 x2) [x1 x2] [x2 x1])
        [y-min y-max] (if (<= y1 y2) [y1 y2] [y2 y1])
        xs (range x-min (inc x-max))
        ys (range y-min (inc y-max))]
    (set (for [x xs
               y ys]
           [x y]))))

;; This resulted in OOME
#_(defn interior-points
  "Given a pair of two points which are opposite corners of a rectangle, return a
  collection of all _interior_ points of that rectangle. That is, points that
  are not along an edge. Note that this may be an empty collection for
  rectangles of width one or two."
  [[[x1 y1] [x2 y2]]]
  (let [[x-min x-max] (if (<= x1 x2) [x1 x2] [x2 x1])
        [y-min y-max] (if (<= y1 y2) [y1 y2] [y2 y1])
        xs (range (inc x-min) x-max)
        ys (range (inc y-min) y-max)]
    (set (for [x xs
               y ys]
           [x y]))))

(defn inside?
  "Predicate: given a point pair that defines a rectangle (opposite corners) and a
  candidate point, return truthy/falsey if the point is inside the rectangle's
  interior (does not include along the rectangle's edge)."
  [[[x1 y1] [x2 y2]] [x y]]
  (let [[x-min x-max] (if (<= x1 x2) [x1 x2] [x2 x1])
        [y-min y-max] (if (<= y1 y2) [y1 y2] [y2 y1])]
    (and (< x-min x x-max) (< y-min y y-max))))

(defn runner
  "Return the area of the largest rectangle that can be formed by two opposite
  corners from the points of the puzzle input."
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

(defn runner2
  "Like part 1, but the largest rectangle has to reside entirely within the
  polygon formed by the points of the puzzle input."
  [input]
  (let [points (mapv parse-point input)
        perimeter-point-pairs
        (partition 2 1 (conj points (first points) ))
        perimeter-points
        (->>
         perimeter-point-pairs
         (map area-points)
         (reduce into))
        points-alist (mapv vector (range) points)
        point-pairs
        (for [[i p1] points-alist
              [j p2] points-alist
              :when (< i j)]
          [p1 p2])]
    (->>
     point-pairs
     ;; Key insight: given a set of all points of the perimeter of the polygon,
     ;; remove any candidate rectangle which has any perimeter point in its
     ;; interior (not along its edge).
     ;; (An earlier attempt OOME'd by attempting to do clojure.set/intersection
     ;; on `(interior-points point-pair perimeter-points)`, since the area of
     ;; the largest rectangles were on the order of 1 billion.)
     (remove (fn [point-pair]
               (some (partial inside? point-pair) perimeter-points)))
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

  (area-points [[7 1] [11 1]])
  (area-points [[11 1] [7 1]])
  (area-points [[2 5] [11 1]])

  ;; (interior-points [[7 1] [11 1]])
  ;; (interior-points [[11 1] [7 1]])
  ;; (interior-points [[2 5] [11 1]])

  (inside? [[7 1] [11 1]] [7 2])
  (inside? [[2 5] [11 1]] [3 3])


  (runner ["7,1"
           "11,1"
           "11,7"
           "9,7"
           "9,5"
           "2,5"
           "2,3"
           "7,3"]) ;; 50

  ;; should yield 24
  (runner2 ["7,1"
            "11,1"
            "11,7"
            "9,7"
            "9,5"
            "2,5"
            "2,3"
            "7,3"]) ;; 24

  (with-open [r (io/reader (io/resource "aoc-2025/day9.txt"))]
    (runner (line-seq r)));; 4750092396

  (time (with-open [r (io/reader (io/resource "aoc-2025/day9.txt"))]
     (runner2 (line-seq r)))) ;; 1468516555
  ;; "Elapsed time: 80583.605375 msecs"

  ;; end comment
  )
