(ns aoc-2023.day3
  "Gear Ratios. Grid of numbers, some of which touch symbols."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [lib.grid :as grid]))

(def digits #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})
(def period \.)
(def star \*)

(defn mappify-number
  "Given an aseq (alist) representing a number (each item is a pair `[digit, index]`), return a single
  map containing the parsed `:number` and the collection of `:indices`."
  [aseq]
  (let [nm (apply str (map first aseq))
        indices (set (map second aseq))]
    {:number (edn/read-string nm)
     :indices indices}))

(defn mappify-number-neighbors
  "Given a number map, add the collection of `:neighbors`, which are the indices neighboring the
  `:number` on the indicated grid."
  [grid-w grid-h {:keys [indices] :as number}]
  (let [neighbors (set/difference
                   (set (mapcat (partial grid/neighbors grid-w grid-h) indices))
                   indices)]
    (assoc number :neighbors neighbors)))

(defn massage
  "Given an aseq (alist) representing a number (each item is a pair `[digit, index]`), calculate the
  neighbors of the index in that grid. Parse the number string into a number. Return a map."
  [grid-w grid-h aseq]
  (->> aseq mappify-number (mappify-number-neighbors grid-w grid-h)))

(defn number-map
  "Given a collection of number maps, return a single map of index to number. So it returns a map of
  every index in the grid which belongs a number, to the number it belongs to."
  [numbers]
  (reduce (fn [val {:keys [number indices]}]
            (apply assoc val (interleave indices (repeat number)))) {} numbers))

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

(defn runner2
  "Similar to part one, find all the numbers, but then build a map of all indices which contain any
  part of a number, to the number. Then, find all the star characters and their indices, find their
  neighboring indices, and use the number map to find only the stars adjacent to exactly two
  numbers. Sum the gear ratios (product of two numbers)."
  [inputs]
  (let [w (grid/width inputs)
        h (grid/height inputs)
        input (apply concat inputs)
        numbers (->> input grid/alist (partition-by (comp nil? digits first)) (filter (comp digits first first)) (map mappify-number))
        nm-map (number-map numbers)
        nms-at-indices (fn [index-seq]
            (reduce (fn [a-set item] (if-let [nm (nm-map item)] (conj a-set nm) a-set)) #{} index-seq))]
    (->>
     input
     grid/alist
     (partition-by (comp #(= star %) first))
     (filter (comp #(= star %) first first))
     (map (comp nms-at-indices (partial grid/neighbors w h) second first))
     (filter #(= 2 (count %)))
     (map #(apply * %))
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

  (runner2 ["467..114.."
            "...*......"
            "..35..633."
            "......#..."
            "617*......"
            ".....+.58."
            "..592....."
            "......755."
            "...$.*...."
            ".664.598.."]) ;; 467835

  (with-open [r (io/reader (io/resource "aoc-2023/day3.txt"))]
    (runner2 (line-seq r))) ;; 84051670

  (number-map [{:number 333 :indices #{2 3 4}} {:number 666 :indices #{8 9 10}}]) ;; {4 333, 3 333, 2 333, 9 666, 10 666, 8 666}

  ;; end comment
  )
