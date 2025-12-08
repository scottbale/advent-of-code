(ns aoc-2025.day4
  "Forklift access to printing paper rolls arranged in a grid."
  (:require
   [clojure.java.io :as io]
   [debugger :refer [dbg]]
   [lib.grid :as grid]))

(defn roll-at-loc?
  "At index i of the grid alist, is there a roll? `grid-alist` must be a vector or
  something that responds to `get` fn."
  [grid-alist i]
  (= \@ (first (get grid-alist i))))

(defn roll-accessible?
  "Is there a roll at location `i` that is accessible by forklift?"
  [w h grid-alist i]
  (when (roll-at-loc? grid-alist i)
    (> 4 (count (filter true? (map (partial roll-at-loc? grid-alist) (grid/neighbors w h i)))))))

(defn runner
  "Find the number of rolls of paper accessible by forklift. A roll is accessible
  if there are fewer than four rolls of paper in the eight adjacent positions."
  [inputs]
  (let [w (grid/width inputs)
        h (grid/height inputs)
        input (apply concat inputs)
        alist (->> input grid/alist vec)
        pred (partial roll-accessible? w h alist)]
    (->>
     alist
     (filter (comp pred second))
     count)))


(comment

  (runner ["..@@.@@@@." 
           "@@@.@.@.@@" 
           "@@@@@.@.@@" 
           "@.@@@@..@."
           "@@.@@@@.@@"
           ".@@@@@@@.@"
           ".@.@.@.@@@"
           "@.@@@.@@@@"
           ".@@@@@@@@."
           "@.@.@@@.@."]) ;; 13

  (with-open [r (io/reader (io/resource "aoc-2025/day4.txt"))]
    (runner (line-seq r))) ;; 1320

  (->> 
   ["..@@.@@@@." 
    "@@@.@.@.@@" 
    "@@@@@.@.@@" 
    "@.@@@@..@."
    "@@.@@@@.@@"
    ".@@@@@@@.@"
    ".@.@.@.@@@"
    "@.@@@.@@@@"
    ".@@@@@@@@."
    "@.@.@@@.@."]
   (apply concat)
   grid/alist
   vec
   )

  ((->> 
    ["..@@.@@@@." 
     "@@@.@.@.@@" 
     "@@@@@.@.@@" 
     "@.@@@@..@."
     "@@.@@@@.@@"
     ".@@@@@@@.@"
     ".@.@.@.@@@"
     "@.@@@.@@@@"
     ".@@@@@@@@."
     "@.@.@@@.@."]
    (apply concat)
    grid/alist
    vec
    (partial roll-at-loc?)) 3)

  ((->> 
    ["..@@.@@@@." 
     "@@@.@.@.@@" 
     "@@@@@.@.@@" 
     "@.@@@@..@."
     "@@.@@@@.@@"
     ".@@@@@@@.@"
     ".@.@.@.@@@"
     "@.@@@.@@@@"
     ".@@@@@@@@."
     "@.@.@@@.@."]
    (apply concat)
    grid/alist
    vec
    (partial roll-accessible? 10 10)) 12)


  ;; end comment
  )
