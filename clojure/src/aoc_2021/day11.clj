(ns aoc-2021.day11
  "Flashing octopi"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [debugger :refer [dbg]]))

(def input->digits
  "Transform a String line of input into a seq of numbers (single digits)"
  (partial map (comp edn/read-string str)))

(defn adjacents
  "Given `w` width and `h` height of a `w` x `h` grid, and `n` one of the spots on the
  grid (zero-based), return the seq of three to eight points adjacent to `n` (including diagonal
  neighbors)"
  [w h n]
  (let [max-n (* w h)]
    (apply concat
           ;; rows (outer) and columns (inner) with nested list comprehensions
           (for [r [(- n w) n (+ n w)]
                 :when (>= r 0)] ;; <-- bug fix :(
             (let [b (* w (long (/ r w))) ;; row beginning
                   e (+ b w -1)]          ;; row ending
               (for [c [(dec r) r (inc r)]
                     :when (and
                            (not (== c n))
                            (>= c 0)
                            (< c max-n)
                            (>= c b)
                            (<= c e))]
                 c))))))

(defn next-gen
  "Calculate the next generation of the grid. `n` is the grid count, `adjacents-seq` is the sequence
  of adjacent indices for each spot in the grid - both of these are 'static' for given grid
  dimensions. The `grid-seq` is the energy levels of the current generation. Returns a seq of the
  next gen."
  [n adjacents-seq grid-seq]
  (let [grid-seq (mapv inc grid-seq)
        indices (range)
        pairs (fn [grid-seq']
                (map vector indices grid-seq'))
        flasher? (partial < 9)
        zero-out (fn [n] (if (flasher? n) 0 n))

        ;; reducer fn
        inc-and-gather-flashed (fn [[flashed-indices gs :as pair] i]
                                 (if (flasher? (nth gs i))
                                   pair
                                   (let [gs' (update gs i inc)
                                         fi' (if (flasher? (nth gs' i))
                                               (conj flashed-indices i)
                                               flashed-indices)]
                                     [fi' gs'])))

        stabilize (fn [grid-seq]
                    (loop [gs grid-seq
                           flasher-pairs (filter (comp flasher? second) (pairs grid-seq))]
                      (let [ ;; these are all the indices that need to be inc'd
                            adjacents-indices (mapcat (comp (partial nth adjacents-seq) first) flasher-pairs)
                            [flashed-indices' gs'] (reduce inc-and-gather-flashed [#{} gs] adjacents-indices)]
                        ;; of the ones just inc'd, did any flash?
                        (if (seq flashed-indices')
                          (recur gs' (map #(vector % (nth gs' %)) flashed-indices'))
                          gs'))))
        ]
    ;; zero out anything > 9
    (map zero-out (stabilize grid-seq))))


(defn runner-pt1
  "Input is a seq of numbers representing two-dimensional height map. What is the total number of
  flashes after 100 steps?"
  [input]
  (let [h (count input)
        w (count (first input))
        n (* w h)
        input (mapcat input->digits input)
        indices (range 0 (* w h))
        adjacents-seq (map (partial adjacents w h) indices)
        nm-steps (range 0 100)
        reducer (fn [[flash-count current-gen] _]
                  (let [gen' (next-gen n adjacents-seq current-gen)
                        flash-count' (+ flash-count (count (filter zero? gen')))]
                    [flash-count' gen']))]
    (first (reduce reducer [0 input] nm-steps))))

(defn runner-pt2
  "During what step do all octopi flash simultaneously?"
  [input]
  (let [h (count input)
        w (count (first input))
        n (* w h)
        input (mapcat input->digits input)
        indices (range 0 (* w h))
        adjacents-seq (map (partial adjacents w h) indices)
        nm-steps (range)
        reducer (fn [[_ current-gen] step-nm]
                  (let [gen' (next-gen n adjacents-seq current-gen)
                        nu-count (count (filter zero? gen'))]
                    (if (== n nu-count)
                      (reduced [step-nm gen'])
                      [step-nm gen'])))]
    (inc (first (reduce reducer [0 input] nm-steps)))))

(comment

  ;; pt 2

  (runner-pt2 ["5483143223"
               "2745854711"
               "5264556173"
               "6141336146"
               "6357385478"
               "4167524645"
               "2176841721"
               "6882881134"
               "4846848554"
               "5283751526"]) ;; 195

  (with-open [r (io/reader (io/resource "aoc-2021/day11.txt"))]
    (runner-pt2 (line-seq r))) ;; 364

  ;; pt 1

  (runner-pt1 ["5483143223"
               "2745854711"
               "5264556173"
               "6141336146"
               "6357385478"
               "4167524645"
               "2176841721"
               "6882881134"
               "4846848554"
               "5283751526"]) ;; 1656

  (with-open [r (io/reader (io/resource "aoc-2021/day11.txt"))]
    (runner-pt1 (line-seq r))) ;; 1743


  ;; dbg 4th->5th gen
  (= (runner-pt1 ["2263031977"
                  "0923031697"
                  "0032221150"
                  "0041111163"
                  "0076191174"
                  "0053411122"
                  "0042361120"
                  "5532241122"
                  "1532247211"
                  "1132230211"])
     (mapcat input->digits ["4484144000"
                            "2044144000"
                            "2253333493"
                            "1152333274"
                            "1187303285"
                            "1164633233"
                            "1153472231"
                            "6643352233"
                            "2643358322"
                            "2243341322"]))

  (adjacents 4 5 5)
  (adjacents 4 5 0)

  ;; dbg
  (adjacents 10 10 9)
  (adjacents 10 10 89)

  ;; 0 1 2
  ;; 3 4 5
  ;; 6 7 8

  (adjacents 3 3 4)
  (adjacents 3 3 7)
  (adjacents 3 3 8)

  (let [w 5
        h 5
        n (* w h)
        indices (range 0 n)
        aj (map (partial adjacents w h) indices)
        input ["11111"
               "19991"
               "19191"
               "19991"
               "11111"]
        input (mapcat input->digits input)
        ]
    (next-gen n aj (next-gen n aj input)))

  (let [input [0 1 2 3 4 5 6 7 8]
        n 3]
    (concat (take n input) (cons :foo (drop (inc n) input))))

  (let [input [0 1 2 3 4 5 6 7 8]
        n 3]
    (assoc input n :foo))

  (let [input [0 1 2 3 4 5 6 7 8]
        n 3]
    (update input n str))

  (require '[clojure.string :as s])
  (let [input [1 1 1 1 1
               1 9 9 9 1
               1 9 1 9 1
               1 9 9 9 1
               1 1 1 1 1]]
    (->> input
         (partition 5)
         (map #(apply str %))
         (s/join "\n")
         print
         ))

  )
