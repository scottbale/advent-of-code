(ns aoc-2025.day10
  "Push buttons to light machines up. Represent input tokens as bytes."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.math.combinatorics :as combo]
   [clojure.string :as s]
   [debugger :refer [dbg]]))

(defn strip
  "Strip brackets bracketing input string token"
  [token]
  (subs token 1 (-> token count dec)))

(defn parse-indicator-light-diagram
  "Parse the first thing in the input into a byte where the `#` pound signs are
  high bits."
  [input-str]
  (->>
   input-str
   strip
   (mapv vector (range))
   (filter (comp (partial = \#) second))
   (mapv first)
   (reduce bit-set (byte 0x00))))

(defn parse-button-wiring-schematic
  "Parse an e.g. `(0,2,3,4)` into a byte where each number is the index of a bit
  that will be set high."
  [input-str]
  (let [input-str (strip input-str)
        nms (map edn/read-string (s/split input-str #","))]
    (reduce bit-set (byte 0x00) nms)))

(defn parse-vector-of-nums
  [token]
  (mapv edn/read-string (->
                         token
                         strip
                         (s/split #","))))

(defn process-one
  "Process one line of input. Each of the tokens in the line of input can be represented as a byte,
  and it becomes a matter of (a) for increasingly larger combinations of the buttons, (b) apply
  `bit-xor` to each combination to see if it yields the expected value from the first token. The
  size of the combination is the 'number of button presses'."
  [input-str]
  (let [inputs (s/split input-str #" ")
        expected (parse-indicator-light-diagram (first inputs))
        buttons (->>
                 inputs
                 rest
                 drop-last
                 (map parse-button-wiring-schematic))]
    (loop [i 1]
      (when (<= i (count buttons)) ;; safety valve, shouldn't be necessary
        ;; https://github.com/clojure/math.combinatorics
        (let [combos (combo/combinations buttons i)]
          (if (some (fn [btns] (= expected (reduce bit-xor btns))) combos)
            i
            (recur (inc i))))))))

(defn step
  [expected buttons wips wip]
  (let [r (reduce
           (fn [wips button]
             (let [wip' (reduce
                         (fn [w i]
                           (let [w (update w i inc)]
                             (cond
                               (= expected w) (reduced :match)              ;; found a match
                               (< (get expected i) (get w i)) (reduced nil) ;; too high, don't add
                               :else w)))
                         wip button)]
               (cond
                 (= :match wip') (reduced :match) ;; found a match
                 (nil? wip') wips                 ;; too high, don't add
                 :else (conj wips wip'))))
           wips buttons)]
    (if (= :match r)
      (reduced r)
      r)))

(defn process-one-pt2
  [input-str]
  (let [inputs (rest (s/split input-str #" "))
        expected (parse-vector-of-nums (last inputs))
        buttons (->>
                 inputs
                 drop-last
                 (map parse-vector-of-nums))
        init (vec (repeat (count expected) 0))]
    (loop [i 1
           wips (set (repeat (count buttons) init))]

      (let [result (reduce (partial step expected buttons) #{} wips)]
        (if (= :match result)
          i ;; found the match
          (recur (inc i) result))))))

(defn runner
  "Sum the result (number of button presses) for each line of input."
  [inputs]
  (->>
   inputs
   (map process-one)
   (reduce + 0)))

(defn runner2
  [inputs]
  (->>
   inputs
   (map process-one-pt2)
   (reduce + 0)))


(comment

  (parse-indicator-light-diagram "[.##.]")
  (parse-indicator-light-diagram "[...#.]")
  (parse-indicator-light-diagram "[.###.#.]")
  (parse-button-wiring-schematic "(0,2,3,4)")
  (parse-vector-of-nums "(0,2,3,4)")

  (bit-xor 0 (byte 0x02) (byte 0x04))

  (process-one "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}")
  (process-one "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}")

  (process-one-pt2 "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}")

  (step [3 5 4 7] '([0 1] [2 3] [3]) #{[1 1 1 1]} [3 5 2 3] )
  (step [3 5 4 7] '([0 1] [2 3] [3]) #{[1 1 1 1]} [3 5 3 6] )


  (runner ["[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
           "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
           "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"]) ;; 7

  (runner2 ["[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
            "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
            "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"]) ;; 33

  (with-open [r (io/reader (io/resource "aoc-2025/day10.txt"))]
    (runner (line-seq r))) ;; 375

  (time (with-open [r (io/reader (io/resource "aoc-2025/day10.txt"))]
     (runner2 (line-seq r))))

  ;; end comment
  )
