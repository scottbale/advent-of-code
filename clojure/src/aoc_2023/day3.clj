(ns aoc-2023.day3
  "Gear Ratios. Grid of numbers, some of which touch symbols."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [debugger :refer [dbg]]
   [lib.grid :as grid]))

(def digits #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
(def period \.)

(defn massage
  "Given an aseq (alist) representing a number (each item is a pair `[digit, index]`), calculate the
  neighbors of the index in that grid. Parse the number string into a number. Return a map."
  [grid-w grid-h aseq]
  (let [nm (apply str (map first aseq))
        indices (set (map second aseq))
        neighbors (set/difference
                   (set (mapcat (partial grid/neighbors grid-w grid-h) indices))
                   indices)]
    {:number (edn/read-string nm)
     :neighbors neighbors}))

(defn a-symbol?
  "Is the character at index i a symbol?"
  [input i]
  (not (= period (nth input i))))

(defn any-symbols?
  "Do any of the number's neighboring indexes contain symbols?"
  [input {:keys [neighbors]}]
  (some (partial a-symbol? input) neighbors))

(defn runner
  "Partition and filter the input into only the numbers, paired with their indices. Calculate the
  neighbor indices of each number. Filter to only numbers which contain a symbol among its neighbor
  indices. Sum remaining numbers."
  [inputs]
  (let [w (grid/width inputs)
        h (grid/height inputs)
        input (apply concat inputs)]
    (->>
     input
     grid/alist
     (partition-by (comp nil? digits first))
     (filter (comp digits first first))
     (map (partial massage w h))
     (filter (partial any-symbols? input))
     (map :number)
     (reduce +))))

(comment

  (runner ["467..114.."
           "...*......"
           "..35..633."
           "......#..."
           "617*......"
           ".....+.58."
           "..592....."
           "......755."
           "...$.*...."
           ".664.598.."]) ;; 4361

  (massage 5 5 '([\3 1] [\4 2] [\8 3]))

  (with-open [r (io/reader (io/resource "aoc-2023/day3.txt"))]
    (runner (line-seq r))) ;; 532428

  ;; end comment
  )
