(ns aoc-2021.day9
  "Smoke settling in low point basins"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as c.set]
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
     (for [c [(- n w) (+ n w)] :when (and (>= c 0) (< c (* w h)))] c))))

(defn low-points
  "Given the input (parsed into a seq of numbers) etc., find and return the points, in
  the form of a pair of numbers [value index]"
  [input indices adjacents-seq]
  (let [nths (partial nth input)]
    (letfn [(min-finder [found [n i adjs]]
              (if (every? (partial < n) (map nths adjs))
                (conj found [n i])
                found))]
      (reduce min-finder [] (map vector input indices adjacents-seq)))))

(defn basin
  "Given the input and a known low point [value index], recursively discover the set of [value index] pairs comprising the basin. "
  [input indices adjacents-seq [n i :as low-point]]
  (letfn [(not-basin? [[n i]] (== n 9))
          (zip-adjacents [adjacents]
            (map #(vector (nth input %) %) adjacents))
          (step [found queue]
            (let [[[val i :as point] & more :as queue] (drop-while not-basin? queue)]
              (if (nil? val)
                found
                (let [adjs (nth adjacents-seq i)
                      ;; make adjs a set of [val i] pairs
                      adjs (set (zip-adjacents adjs))
                      adjs (c.set/difference adjs found)]
                  (recur (conj found point) (into more adjs))))))]
    (step #{low-point} (set (zip-adjacents (nth adjacents-seq i))))))

(defn runner-pt1
  "Input is a seq of numbers representing two-dimensional height map"
  [input]
  (let [h (count input)
        w (count (first input))
        input (mapcat input->digits input)
        indices (range 0 (* w h))
        adjacents-seq (map (partial adjacents w h) indices)]
    (->> (low-points input indices adjacents-seq)
         (map (comp inc first))
         (reduce +))))

(defn runner-pt2
  "Find the product of the sizes of the top three largest basins."
  [input]
  (let [h (count input)
        w (count (first input))
        input (mapcat input->digits input)
        indices (range 0 (* w h))
        adjacents-seq (map (partial adjacents w h) indices)
        low-point-pairs (low-points input indices adjacents-seq)
        basin-f (partial basin input indices adjacents-seq)]
    (->> low-point-pairs
         (map (comp count basin-f))
         sort
         reverse
         (take 3)
         (reduce *))))

(comment

  ;; pt 2

  (runner-pt2 ["2199943210"
               "3987894921"
               "9856789892"
               "8767896789"
               "9899965678"]) ;; 1134

  (with-open [r (io/reader (io/resource "aoc-2021/day9.txt"))]
    (runner-pt2 (line-seq r))) ;; 891684

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
  (adjacents 100 100 9998)
  ;; blammo!
  (adjacents 100 100 9900)
  ;; blammo!
  (adjacents 10 10 90)
  ;; blammo!
  (adjacents 3 3 6)
  ;; blammo!
  (adjacents 2 2 2)


  ;; Had to use this to debug how an adjacent was overflowing the size of the grid
  ;; Identified the problem with `(adjacents 100 100 9900)`
  ;; bug: `<=` not `<` in second `for` form of `adjacents`
  (let [w 100
        h 100
        indices (range 0 (* w h))
        adjs (map #(vector % (adjacents w h %)) indices)
        ]
    (filter
     (fn [[n adjs]]
       (some (partial == 10000) adjs))
     adjs))

  
  (let [input [1 2 3 4 5]]
    (reduce + (map (partial nth input) #{2 0 3})))

  (let [input [1 2 3 4 5]]
    ((partial transduce (map (partial nth input)) +) #{2 0 3}))

  some-fn
  some
  every?

  )
