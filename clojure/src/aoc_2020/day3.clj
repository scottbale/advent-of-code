(ns aoc-2020.day3
  (:require
   [clojure.java.io :as io]
   [debugger :refer [dbg]]))

(def filter-fn {\# true     ;; tree
                \. false    ;; open
                })

(defn tree-count
  [slope-x slope-y input-width input]
  (let [indices (map #(mod (* slope-x %) input-width) (range))
        lines (take-nth slope-y input)
        path (map nth lines indices)]
    (count (filter filter-fn path))))

(comment

  (let [input ["..##......."
               "#...#...#.."
               ".#....#..#."
               "..#.#...#.#"
               ".#...##..#."
               "..#.##....."
               ".#.#.#....#"
               ".#........#"
               "#.##...#..."
               "#...##....#"
               ".#..#...#.#"]
        input-width 11
        slopes [[1 1] [3 1] [5 1] [7 1] [1 2]]]
    (map (fn [[slope-x slope-y]] 
           (tree-count slope-x slope-y input-width input)) 
         slopes))

  (with-open [r (io/reader (io/resource "aoc-2020/day3.txt"))]
    (let [input (line-seq r)
          input-width 31
          slopes [[1 1] [3 1] [5 1] [7 1] [1 2]]]
      (apply * (map (fn [[slope-x slope-y]] 
                      (tree-count slope-x slope-y input-width input)) 
                    slopes))
      )) ;; 9406609920


  )
