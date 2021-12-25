(ns aoc-2021.day13
  "Folding transparent sheet of points"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as s]
   [debugger :refer [dbg]]))

(def axis->index {"y" 1 "x" 0})

(defn parse-point [input]
  (edn/read-string (str \[ input \])))

(defn parse-fold [input]
  (let [[axis value] (s/split (last (s/split input #" ")) #"=")]
    [(axis->index axis) (edn/read-string value)]))

(defn fold-a-nm
  "Given the fold val, update the folded `i`. When `i` is greater than the val, it is 'folded' back."
  [val i]
  (if (<= i val)
    i
    (- (* 2 val) i)))

(defn fold-a-point
  "A point is a vector pair [x,y]. A fold is an index into that pair (zero or one) and a value. Update
  the x or y of the point according to the fold."
  [[index val] point]
  (update point index (partial fold-a-nm val)))

(defn runner-pt1
  "Apply the first fold to the set of points, return point count"
  [input]
  (let [[points _ folds] (partition-by empty? input)
        points (set (map parse-point points))
        folds (map parse-fold folds)]
    (count (set (map (partial fold-a-point (first folds)) points)))))

(defn pretty-print
  "Pretty-print the final grid of points, after all folds have been applied"
  [folds points]
  (let [max-x (second (last (filter (comp zero? first) folds)))
        max-y (second (last (filter (comp pos? first) folds)))
        grid (vec (repeat max-y (vec (repeat max-x \.))))
        grid-fn (fn [grid [x y]]
                  (update grid y #(assoc % x \#)))
        grid (reduce grid-fn grid points)]
    (println (s/join "\n" (map #(apply str %) grid)))))

(defn runner-pt2
  "Apply all the folds to the input set of points. What 8 letters do the points spell out?"
  [input]
  (let [[points _ folds] (partition-by empty? input)
        points (set (map parse-point points))
        folds (map parse-fold folds)]
    (pretty-print
     folds
     (reduce
      (fn [pts fold]
        (set (map (partial fold-a-point fold) pts)))
      points folds))))


(comment

  ;; pt 2

  (runner-pt2 ["6,10"
               "0,14"
               "9,10"
               "0,3"
               "10,4"
               "4,11"
               "6,0"
               "6,12"
               "4,1"
               "0,13"
               "10,12"
               "3,4"
               "3,0"
               "8,4"
               "1,10"
               "2,14"
               "8,10"
               "9,0"
               ""
               "fold along y=7"
               "fold along x=5"])

  (with-open [r (io/reader (io/resource "aoc-2021/day13.txt"))]
    (runner-pt2 (line-seq r))) ;; PERCGJPB

  ;; pt 1

  (runner-pt1 ["6,10"
               "0,14"
               "9,10"
               "0,3"
               "10,4"
               "4,11"
               "6,0"
               "6,12"
               "4,1"
               "0,13"
               "10,12"
               "3,4"
               "3,0"
               "8,4"
               "1,10"
               "2,14"
               "8,10"
               "9,0"
               ""
               "fold along y=7"
               "fold along x=5"]) ;; 17

  (with-open [r (io/reader (io/resource "aoc-2021/day13.txt"))]
    (runner-pt1 (line-seq r))) ;; 781

  (parse-point "6,12")
  (parse-fold "fold along y=7")
  (map (partial fold-a-nm 5) (range 12))

  )
