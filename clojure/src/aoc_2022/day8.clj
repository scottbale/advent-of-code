(ns aoc-2022.day8
  "Treetop tree house - heights in a grid"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [debugger :refer [dbg]]))

(defn bounds
  "Given a width `w` and height `h` defining a w x h grid, and an index `i`, return a sequence of four
  vectors of indices into the grid. These are the four paths from `i` to each of the four edges.
  They are each ordered with `i` as the first index in the sequence. Each sequence will have at
  least one value."
  [w h i]
  (let [n (* w h)
        row (long (/ i w))
        x0 (* w row)
        xn (+ x0 w)
        col (mod i w)
        yn (+ (* w (dec h)) col)]
    [(reverse (range x0 (inc i)))
     (range i xn)
     (reverse (range col (inc i) w))
     (range i (inc yn) w)]))

(defn tallest?
  "Given a `grid` and a `path` from `i` to an edge, is the grid value at index `i` the tallest value
  in that path?"
  [grid [i next-i :as path]]
  (if (nil? next-i)
    true
    (let [i-val (get grid i)
          other-vals (map #(get grid %) (rest path))
          tallest-other-val (apply max other-vals)]
      (> i-val tallest-other-val))))

(defn scenic-seq
  "Given a tree height `h` and a seq of adjacent trees with heights `hs`, return the sequence of
  visible trees from `h`. The sequence is up to, and including, the first tree that is as tall as or
  taller than `h`."
  ;; Copied from `take-while`
  [h hs]
  (lazy-seq
   (when-let [s (seq hs)]
     (cond
       (> h (first s)) (cons (first s) (scenic-seq h (rest s)))
       :else (take 1 s)))))

(defn scenic-score
  "Given a `grid` and a `path` from `i` to an edge, calculate the 'scenic score' of the grid value at index `i`"
  [grid [i next-i :as path]]
  (if (nil? next-i)
    0
    (let [i-val (get grid i)
          other-vals (map #(get grid %) (rest path))
          ]
      (count (scenic-seq i-val other-vals)))))

(defn runner1 ;; pt 1
  "How many trees are visible from outside the map?"
  [input]
  (let [width (-> input first count)
        height (count input)
        n (* width height)
        grid (vec (mapcat #(map (comp edn/read-string str) %) input))
        visible (vec (repeat 25 false))
        indices (range 0 n)
        reduce-fn (fn [m i]
                    (let [bs (bounds width height i)]
                      (assoc m i (or (some (partial tallest? grid) bs) false))))]
    (->> indices
         (reduce reduce-fn visible)
         (filter true?)
         count)))

(defn runner ;; pt 2
  "Find the maximum 'scenic score' of the trees in the grid."
  [input]
  (let [width (-> input first count)
        height (count input)
        n (* width height)
        grid (vec (mapcat #(map (comp edn/read-string str) %) input))
        scores (vec (repeat 25 0))
        indices (range 0 n)
        reduce-fn (fn [m i]
                    (let [bs (bounds width height i)]
                      (assoc m i (apply * (map (partial scenic-score grid) bs)))))]
    (->> indices
         (reduce reduce-fn scores)
         (apply max))))


(comment

  ;; a grid of heights, 0 shortest to 9 tallest
  (runner ["30373" 
           "25512" 
           "65332" 
           "33549" 
           "35390"]) ;; 8 ;; 21

  (with-open [r (io/reader (io/resource "aoc-2022/day8.txt"))]
    (runner (line-seq r)))
  ;; pt 2: 235200 ;; 119 too low
  ;; pt 1: 1825 ;; 3346 too high

  (let [grid [3 0 3 7 3
              2 5 4 5 2
              6 5 3 5 2
              3 3 5 4 9
              3 5 3 9 0]]
    (tallest? grid [11 12 13 14]))

  (let [grid [3 0 3 7 3 
              2 5 4 5 2 
              6 5 3 5 2 
              3 3 5 4 9 
              3 5 3 9 0]]
    (scenic-score grid [11 12 13 14])
    ;;(scenic-score grid [11 10])
    ;;(scenic-score grid [10])
    )

  (scenic-seq 8 [7 8 9 9])
  (scenic-seq 8 [7 9 9])
  (scenic-seq 8 [7 8])
  (scenic-seq 8 [7 8 9])
  (scenic-seq 8 [7 7 9])
  (scenic-seq 8 [7 7])
  (scenic-seq 8 [7 6 8 9])

  (let [grid [3 0 3 7 3 
              2 5 5 1 2 
              6 5 3 3 2 
              3 3 5 4 9 
              3 5 3 9 0]]
    (tallest? grid [10]))

  (let [grid [3 0 3 7 3 
              2 5 5 1 2 
              6 5 3 3 2 
              3 3 5 4 9 
              3 5 3 9 0]]
    (map #(get grid %) [12 13 14]))


  (let [w 5
        h 5
        i 15]
    (bounds w h i))

  (update [0 1 2 3] 1 inc)
  (update (vec (list 0 1 2 3)) 1 inc)

  (let [w 5
        h 5
        n (* w h)
        grid [3 0 3 7 3 
              2 5 5 1 2 
              6 5 3 3 2 
              3 3 5 4 9 
              3 5 3 9 0]
        visible (vec (repeat 25 false))
        indices (range 0 n)]
    (letfn [(foo' [w h g m i]
              (let [bs (bounds w h i)]
                (assoc m i (or (some (partial tallest? g) bs) false))
                ))
            ]
      ;; (bounds w h 10)
      ;; (tallest? grid [10])
      (reduce (partial foo' w h grid) visible indices)
      ))

  )
