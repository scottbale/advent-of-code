(ns aoc-2025.day10
  "Push buttons to light machines up."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as s]
   [debugger :refer [dbg]]))

(defn parse-indicator-light-diagram
  "Parse the first thing in the input into a byte where the `#` pound signs are
  high bits."
  [input-str]
  (->>
   input-str
   rest
   reverse
   rest
   (mapv vector (range))
   (filter (comp (partial = \#) second))
   (mapv first)
   (reduce bit-set (byte 0x00))))

(defn parse-button-wiring-schematic
  "Parse an e.g. `(0,2,3,4)` into a byte where each number is the index of a bit
  that will be set high."
  [input-str]
  (let [input-str (subs input-str 1 (-> input-str count dec))
        nms (map edn/read-string (s/split input-str #","))]
    (reduce bit-set (byte 0x00) nms)))

(defn process-one
  "Process one line of input"
  [input-str]
  (let [inputs (s/split input-str #" ")
        expected (parse-indicator-light-diagram (first inputs))
        buttons (mapv parse-button-wiring-schematic (-> inputs rest drop-last))]
    [expected buttons]
    (loop [i 1]
      )))

(defn runner
  "runner docstring"
  [inputs]
  )


(comment

  (parse-indicator-light-diagram "[.##.]")
  (parse-indicator-light-diagram "[...#.]")
  (parse-indicator-light-diagram "[.###.#.]")
  (parse-button-wiring-schematic "(0,2,3,4)")

  (process-one "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}")

  (runner ["[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
           "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
           "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"])

  (with-open [r (io/reader (io/resource "aoc-2025/day10.txt"))]
    (runner (line-seq r)))

  ;; end comment
  )
