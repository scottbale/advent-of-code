(ns aoc-2025.day8
  "docstring"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as s]
   [debugger :refer [dbg]]))

(defn parse-pt
  "Given a line of input, return a vector triple of three integer coordinates"
  [input]
  (mapv edn/read-string (s/split input #",")))

(defn d
  "Straight-line distance between two points"
  [[x1 y1 z1]
   [x2 y2 z2]]
  ;; (-> 30 biginteger (.pow 2) (+ 3) bigdec (.sqrt MathContext/DECIMAL32))
  (let [dx (- x1 x2)
        dy (- y1 y2)
        dz (- z1 z2)]
    (Math/sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))

(defn circuit-step
  "Reducing step function for collecting circuits. Takes a circuit map (keys are
  points, values are sets of points) and a point triple (a vector triple
  of distance, point1, point2) and returns a possibly-updated circuit map."
  [circuit-map [_d p1 p2 :as _triple]]
  (let [mega-circuit (conj (into (get circuit-map p1 #{})
                                 (get circuit-map p2 #{}))
                           p1 p2)]
    (reduce
     (fn [cm p]
       (assoc cm p mega-circuit))
     circuit-map
     mega-circuit)))

(defn runner
  "Input is a sequence of 'x,y,z' coordinates. `N` is the number of shortest
  distances (connections) to discover. Returns the product of the count of the
  number of M largest graphs resulting from the `N` shortest connections."
  [N input]
  (let [points (->> input (map parse-pt))
        points-alist (->> points (map vector (range)))
        pairs ;; and distances
        (for [[i pt1] points-alist
              [j pt2] points-alist
              :when (< i j)]
          [(d pt1 pt2) pt1 pt2])
        initial-circuit-map (dbg (reduce (fn [m k] (assoc m k #{k})) {} points))]
    (->>
     pairs
     (sort-by first)
     (take N)
     (reduce circuit-step initial-circuit-map)
     vals
     (map count)
     (apply sorted-set-by (fn [x y] (compare y x)))
     (take 3)
     (reduce * 1))))

(defn runner2
  "Like part 1 but take until there is only one large circuit, then return the product of the x
  coordinates of the final pair. I'm going to leave the `N` param and just manually do a binary
  search until I find the right `N`."
  [N input]
  (let [points (->> input (map parse-pt))
        points-alist (->> points (map vector (range)))
        pairs ;; and distances
        (for [[i pt1] points-alist
              [j pt2] points-alist
              :when (< i j)]
          [(d pt1 pt2) pt1 pt2])
        initial-circuit-map (reduce (fn [m k] (assoc m k #{k})) {} points)
        sorted-N (->>
                  pairs
                  (sort-by first)
                  (take N))]
    #_(->>
     sorted-N
     (reduce circuit-step initial-circuit-map)
     vals
     set
     count)
    (let [[_ [x1] [x2]] (last sorted-N)]
      (* x1 x2))))

(comment

  (parse-pt "162,817,812") ;; [162 817 812]

  (d [162 817 812] [57 618 57]) ;; 787.814064357828

  (runner 10 ["162,817,812"
              "57,618,57"
              "906,360,560"
              "592,479,940"
              "352,342,300"
              "466,668,158"
              "542,29,236"
              "431,825,988"
              "739,650,466"
              "52,470,668"
              "216,146,977"
              "819,987,18"
              "117,168,530"
              "805,96,715"
              "346,949,466"
              "970,615,88"
              "941,993,340"
              "862,61,35"
              "984,92,344"
              "425,690,689"]) ;; 40

  (runner2 29 ["162,817,812"
               "57,618,57"
               "906,360,560"
               "592,479,940"
               "352,342,300"
               "466,668,158"
               "542,29,236"
               "431,825,988"
               "739,650,466"
               "52,470,668"
               "216,146,977"
               "819,987,18"
               "117,168,530"
               "805,96,715"
               "346,949,466"
               "970,615,88"
               "941,993,340"
               "862,61,35"
               "984,92,344"
               "425,690,689"])

   (time (with-open [r (io/reader (io/resource "aoc-2025/day8.txt"))]
      (runner 1000 (line-seq r)))) ;; 121770
   ;; 24 wrong, reverse sort order

   (time (with-open [r (io/reader (io/resource "aoc-2025/day8.txt"))]
      (runner2 5674 (line-seq r)))) ;; 7893123992


   ;; list comprehension
   (let [ns (range 5)]
     (for [i ns
           j ns
           :when (< i j)]
       [i j]))

   ;; initial pt 1 brainstorming, now outdated (optimizations proved unnecessary)
   ;;
   ;; * A "junction box" could be a triple of three nums (a point)
   ;; * Every pair of junction boxes has a straight-line distance between them.
   ;; * A circuit is is one or more junction boxes, and their connections (a graph) (_only_ the minimum connections, not fully-connected)
   ;; * Sequence of NxN (the input list zipped with itself into all possible pairs)
   ;;   * association-list of these pairs with their distance, or memoize the function that computes the distance, given two points
   ;; * Only need the sorted list of the first N connections (10 of the sample input, 1000 of the real input)
   ;; * Sort the list of pairs of points by increasing distance.
   ;;   * Possible optimization: Only the first N pairs need to be sorted.
   ;;     Pairs with a greater distance could be pre-filtered before sorting
   ;;     (but that filtering requires the distance value of the Nth pair)
   ;;     In this case N is multiple orders of magnitude smaller than the total
   ;;     count of the list of pairs. Interwebs recommend:
   ;;     * Build a min heap of the 1st N pairs, requires iterating over the
   ;;       entire input list L - O(L)
   ;;     * Sort the contents of the min heap - O(NlogN)
   ;;     * The `java.util.PriorityQueue` class is a queue that orders elements
   ;;       based on their natural ordering by default, which is a min-heap.

   ;; end comment
   )
