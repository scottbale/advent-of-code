(ns aoc-2025.day10
  "Push buttons to light machines up. Represent input tokens as bytes."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.math.combinatorics :as combo]
   [clojure.string :as s]
   [debugger :refer [dbg]]))
;; https://www.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/

(defn strip
  "Strip brackets bracketing input string token"
  [token]
  (subs token 1 (-> token count dec)))

(defn parse-vector-of-nums
  [token]
  (mapv edn/read-string (->
                         token
                         strip
                         (s/split #","))))

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

(defn min-presses
  "Given the sequence of buttons (each a byte), and a target (a byte), calculate the minimum number of
  button presses that can achieve the target using some combination of button presses. Recursively,
  for increasingly larger combinations of the buttons, apply `bit-xor` to each combination to see if
  it yields the target byte value."
  [buttons target]
  (loop [i 1]
    (when (<= i (count buttons)) ;; safety valve, shouldn't be necessary
      ;; https://github.com/clojure/math.combinatorics
      (let [combos (combo/combinations buttons i)]
        (if (some (fn [btns] (= target (reduce bit-xor btns))) combos)
          i
          (recur (inc i)))))))

(defn press-combinations
  "Given the sequence of buttons (each a byte) and a target byte, return a
  collection of every combination of button presses that yields the target byte.
  A combination will contain no duplicate buttons (since a button pressed twice
  has no effect). The largest combination size possible is the size of the
  buttons collection param. The return collection contains collections of
  bytes."
  [buttons target]
  (let [n (count buttons)]
    (loop [i 1
           results #{}]
      (let [combos (combo/combinations buttons i)
            results (->>
                     combos
                     (filter (fn [btns] (= target (reduce bit-xor btns))))
                     (into results))]
        (if (< i n)
          (recur (inc i) results)
          results)))))

(defn next-byte
  "Given a vector of N numbers, return the next byte (which being subtracted from
  the N numbers would leave all even digits).

  E.g. given [3 5 4 7], take digits [1 1 0 1] to return (0x11) (leaving [2 4 4 6])"
  [numbers]
  (->>
   numbers
   (map
    (fn [i digit]
      (if (even? digit)
        [i 0]
        [i 1]))
    (range))
   (reduce
    (fn [result-byte [i bit]]
      (if (< 0 bit) (bit-set result-byte i) result-byte))
    (byte 0x00))))

(defn decrement-numbers
  "Given a combination of buttons (coll of bytes) and an N-sized vector of
  numbers, return the N-sized vector of smaller numbers which, if the buttons
  were applied to, would yield the `numbers` param. Return `nil` if there is no
  possible answer."
  [buttons numbers]
  (let [n (count numbers)
        indices (range 0 n)]
    (reduce
     (fn [numbers' button]
       (let [r (mapv (fn [n i] (if (bit-test button i) (dec n) n)) numbers' indices)]
         (if (some neg? r) (reduced nil) r)))
     numbers
     buttons)))

(defn half
  "Given an _even_ number, return a number which is half of that number."
  [digit]
  (-> digit (bit-shift-right 1)))

(defn recursive-step
  "Accepts

   * buttons (static, coll of bytes)
   * remaining (vector of N numbers - the joltages)

  Returns the number of button presses

  At each step:
   * Derive the target byte from the remaining joltages buttons
   * Calculate the set of press-combinations which yield that target
   * For each combination:
     * Calculate the (smaller) remaining N numbers which, when the
       combination is applied, yields the current N numbers. All
       digits should be even.
     * Divide new remaining in half.
     * Call recursively, multiply result by two.
   * Take the minimum result of all recursive calls, add to that the
     count of the combination for that recursive call, return that
     sum."
  [buttons remaining]
  (let [target (next-byte remaining)
        combos (press-combinations buttons target)]
    (->>
     combos
     (mapv (fn [c]
             (let [rem' (decrement-numbers c remaining)]
               (cond
                 (nil? rem') nil
                 (some pos? rem')
                 (+ (count c) (* 2 (recursive-step buttons (mapv half rem'))))
                 :else (count c)))))
     (remove nil?)
     (apply min))))

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
    (min-presses buttons expected)))

#_(defn num-vector->bytes
  "Given a vector of numbers of size N (the joltage requirements), return a sequence of N-digit bytes."
  [num-vector]
  (loop [result-bytes []
         remaining-nums num-vector
         i 0]
    (let [[next-byte next-remaining]
          (->>
           remaining-nums
           (map
            (fn [i digit]
              (if (even? digit)
                [i 0 (half digit)]
                [i 1 (half (dec digit))]))
            (range))
           (reduce
            (fn [[result-byte result-nums] [i bit digit]]
              (let [result-byte (if (< 0 bit) (bit-set result-byte i) result-byte)
                    result-nums (conj result-nums digit)]
                [result-byte result-nums]))
            [(byte 0x00) []]))
          result-bytes (conj result-bytes next-byte)
          recur? (and (some pos? next-remaining) (< i 10))]
      (if recur?
        (recur result-bytes next-remaining (inc i))
        result-bytes))))

(defn process-one-pt2
  "Process one line of input for part 2."
  [input-str]
  (let [inputs (rest (s/split input-str #" "))
        joltages (parse-vector-of-nums (last inputs))
        buttons (->>
                 inputs
                 drop-last
                 (map parse-button-wiring-schematic))]
    (recursive-step buttons joltages)))

(defn runner
  "Sum the result (number of button presses) for each line of input."
  [inputs]
  (->>
   inputs
   (map process-one)
   (reduce + 0)))

(defn runner2
  "Sum the result (number of button presses) for each line of input."
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
  (parse-button-wiring-schematic "(3)") 

  (bit-xor 0 (byte 0x02) (byte 0x04))

  (process-one "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}")
  (process-one "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}")
  (min-presses [(byte 0x08) (byte 0x10) (byte 0x04) (byte 0x12) (byte 0x05) (byte 0x03)] (byte 0x06))
  (min-presses [(byte 0x08) (byte 0x10) (byte 0x04) (byte 0x12) (byte 0x05) (byte 0x03)] (byte 0x20))

  ;; --------------------------------------------------------------------------
  ;; First line of example input
  ;; should yield 10
  (process-one-pt2 "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}") ;; 10
  ;; (num-vector->bytes [3 5 4 7]) ;; [11 9 14]
  (min-presses '(8 10 4 12 5 3) (byte 11)) ;; 2
  (min-presses '(8 10 4 12 5 3) (byte 9)) ;; 2
  (min-presses '(8 10 4 12 5 3) (byte 14)) ;; 2
  "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
  "P(3,5,4,7)"
  "P(1,1,0,1) + P(2,4,4,6)"
  "P(1,1,0,1) + 2*P(1,2,2,3)"
  "P(1,1,0,1) + 2*[P(1,0,0,1) + 2*P(0,1,1,1)]"

  (next-byte [3 5 4 7]) ;; 11
  (next-byte [7 5 12 7 2]) ;; 11
  (next-byte [10,11,11,5,10,5]) ;; 46
  (next-byte [1 0 1 1]) ;; 13
  (decrement-numbers #{8 3} [3 5 4 7]) ;; [2 4 4 6]
  (decrement-numbers #{10 4 5} [3 5 4 7]) ;; [2 4 2 6]
  (decrement-numbers #{8 10 12 5} [3 5 4 7]) ;; [2 4 2 4]
  (decrement-numbers #{8 3} [1 0 1 1]) ;; nil
  (decrement-numbers #{8 3} [1 1 0 1]) ;; [0 0 0 0]

  "Configuring the first machine's counters requires a minimum of 10 button
  presses. One way to do this is by pressing (3) once, (1,3) three times, (2,3)
  three times, (0,2) once, and (0,1) twice."
  (combo/combinations #{8 10 4 12 5 3} 2)
  (press-combinations '(8 10 4 12 5 3) (byte 11)) ;;#{(8 10 12 5) (4 12 3) (8 3) (10 4 5)}
  "Pressing {3}, {0, 1}.                [aka 8 3]
   Pressing {1, 3}, {2}, {0, 2}.        [aka 10 4 5]
   Pressing {2}, {2, 3}, {0, 1}.        [aka 4 12 3]
   Pressing {3}, {1, 3}, {2, 3}, {0, 2} [aka 8 10 12 5]"

  (recursive-step '(8 10 4 12 5 3) [1 0 1 1]) ;; 2
  (recursive-step '(8 10 4 12 5 3) [1 1 0 1]) ;; 2
  (recursive-step '(8 10 4 12 5 3) [3 5 4 7]) ;; 10
  ;; --------------------------------------------------------------------------

  ;; should be 12
  (process-one-pt2 "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}") ;; 12
  ;; (num-vector->bytes [7 5 12 7 2]) ;; [11 25 15 4]
  (min-presses '(29 12 17 7 30) (byte 11)) ;; 2
  (min-presses '(29 12 17 7 30) (byte 25)) ;; 2
  (min-presses '(29 12 17 7 30) (byte 15)) ;; 2
  (min-presses '(29 12 17 7 30) (byte 4))  ;; 3

  (recursive-step '(29 12 17 7 30) [7 5 12 7 2])

  ;; (num-vector->bytes [44,35,48,43,24,44])

  ;; --------------------------------------------------------------------------
  (process-one-pt2 "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")


  (runner ["[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
           "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
           "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"]) ;; 7

  (runner2 ["[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
            "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
            "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"]) ;; 34

  (with-open [r (io/reader (io/resource "aoc-2025/day10.txt"))]
    (runner (line-seq r))) ;; 375

  

  ;; end comment
  )
