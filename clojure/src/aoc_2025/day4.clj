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

(defn runner2
  "Recursively remove the removable paper rolls, generating a new grid, until no
  more rolls can be removed; return the total count of removed rolls."
  [inputs]
  (let [w (grid/width inputs)
        h (grid/height inputs)
        input (apply concat inputs)
        remover (fn [an-alist indices]
                  (reduce (fn [the-alist i]
                            (assoc the-alist i [\. i])) an-alist (map second indices)))]
    (loop [alist (->> input grid/alist vec)
           counts 0]
      (let [pred (partial roll-accessible? w h alist)
            removeds (->>
                      alist
                      (filter (comp pred second)))
            removed-count (count removeds)
            counts (+ counts removed-count)]
        (if (= 0 removed-count)
          counts
          (recur (remover alist removeds) counts))))))


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

  (runner2 ["..@@.@@@@."
            "@@@.@.@.@@"
            "@@@@@.@.@@"
            "@.@@@@..@."
            "@@.@@@@.@@"
            ".@@@@@@@.@"
            ".@.@.@.@@@"
            "@.@@@.@@@@"
            ".@@@@@@@@."
            "@.@.@@@.@."]) ;; 43

  (with-open [r (io/reader (io/resource "aoc-2025/day4.txt"))]
    (runner (line-seq r))) ;; 1320

  (with-open [r (io/reader (io/resource "aoc-2025/day4.txt"))]
    (runner2 (line-seq r))) ;; 8354

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
