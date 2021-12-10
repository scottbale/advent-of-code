(ns aoc-2021.day9
  "Smoke settling in low point basins"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [debugger :refer [dbg]]))

(def input->digits
  "Transform a String line of input into a seq of numbers (single digits)"
  (partial map (comp edn/read-string str)))

(defn adjacents
  "Given `w` width and `h` height of a `w` x `h` grid, and `n` one of the spots on the
  grid (zero-based), return the seq of two to four points adjacent to `n`"
  [w h n]
  (let [b (* w (long (/ n w))) ;; begin row
        e (+ b w -1)] ;; end row
    (concat
     (for [r [(dec n) (inc n)] :when (and (>= r b) (<= r e))] r) 
     (for [c [(- n w) (+ n w)] :when (and (>= c 0) (<= c (* w h)))] c))))

(defn runner-pt1
  "Input is a seq of numbers representing two-dimensional height map"
  [input]
  (let [h (count input)
        w (count (first input))
        ;; _ (println ">>>>w" w "h" h)
        input (mapcat input->digits input)
        adjacents-seq (map (partial adjacents w h) (range 0 (* w h)))
        nths (partial nth input)]
    ;; returns a seq of numbers which are low points in the input
    (letfn [(min-finder [found [n adjs]]
              ;; (println ">>>>checking" n "with adjacent indices" adjs)
              (if (every? (partial < n) (map nths adjs))
                (conj found n)
                found))]
      (->> (reduce min-finder [] (map vector input adjacents-seq))
           (map inc)
           (reduce +)))))





(comment


  ;; pt 1

  (runner-pt1 ["2199943210"
               "3987894921"
               "9856789892"
               "8767896789"
               "9899965678"]) ;; 15

  (with-open [r (io/reader (io/resource "aoc-2021/day9.txt"))]
    (runner-pt1 (line-seq r))) ;; 566

  (input->digits "2199943210")
  (adjacents 10 5 27)
  ;; [7 2] adjacents are [6 2] [8 2] [7 1] [7 3]
  ;; [9 2] adjacents are [8 2] [9 1] [9 3] b/c [10 2] is out of bounds

  (let [w 4
        h 6
        n 10] ;; row begins 0 4 8 12 ... row ends 3 7 11 15
    (let [    ;; row (mod n w)
          ;; col (long (/ n w))
          ;; row-neighbors (range (max 0 (dec row)) (inc (min )))
          row-begin 
          row-end
          ]))

  ;;  0  1  2  3 
  ;;  4  5  6  7
  ;;  8  9 10 11
  ;; 12 13 14 15
  ;; 16 17 18 19
  (adjacents 4 5 5)
  (adjacents 4 5 0)
  (adjacents 4 5 19)
  (adjacents 4 5 11)
  (adjacents 4 5 12)
  (adjacents 10 5 5)
  (range 0 (* 4 5))

  
  (let [input [1 2 3 4 5]]
    (reduce + (map (partial nth input) #{2 0 3})))

  (let [input [1 2 3 4 5]]
    ((partial transduce (map (partial nth input)) +) #{2 0 3}))

  some-fn
  some
  every?

  )
