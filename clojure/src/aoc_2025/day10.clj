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

(defn halving
  "Given a vector of digits, return a vector pair having a coefficient of doubling
  and the digits halved 'coefficient' times, until there is at least one odd
  digit."
  [digits-vec]
  (loop [i 1
         digits-vec' digits-vec]
    (if (every? even? digits-vec')
      (recur (* 2 i) (mapv half digits-vec'))
      [i digits-vec'])))

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
  [i buttons remaining]
  (let [indent (fn [i] (apply str (repeat i " ")))
        tab (fn [i] (+ 4 i))
        target (next-byte remaining)
        combos (press-combinations buttons target)
        ;; _ (println (indent i) "---------------------------------------")
        ;; _ (println (indent i) (format "%d combos, considering remaining %s..." 
        ;;                               (count combos)
        ;;                               (str remaining)))
        combo-counts
        (->>
         combos
         (mapv (fn [j c]
                 (let [rem' (decrement-numbers c remaining)
                       res (cond
                             (nil? rem') nil
                             (some pos? rem')
                             (let [[n rem''] (halving rem')
                                   rec-res (recursive-step (tab i) buttons rem'')]
                               (if (nil? rec-res)
                                 nil
                                 (+ (count c) (* n rec-res))))
                             :else (count c))]
                   #_(println 
                    (indent i) 
                    (format "%d) Combo %s yielded %s resulting in %d" j (into [] c) rem' res))
                   res)) (rest (range)))
         (remove nil?))
        
        result (if (seq combo-counts) (apply min combo-counts) nil)
        ]
    #_(println (indent i) (format "...count for remaining %s: %d" (str remaining) result))
    result))

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

(defn process-one-pt2
  "Process one line of input for part 2."
  [input-str]
  (let [inputs (rest (s/split input-str #" "))
        joltages (parse-vector-of-nums (last inputs))
        buttons (->>
                 inputs
                 drop-last
                 (map parse-button-wiring-schematic))]
    (recursive-step 0 buttons joltages)))

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
   (remove nil?)
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

  (recursive-step 0 '(29 12 17 7 30) [7 5 12 7 2])

  ;; (num-vector->bytes [44,35,48,43,24,44])

  ;; --------------------------------------------------------------------------
  ;; should be 11
  (process-one-pt2 "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")
  ;; [0 1 2 3 4] -> 31     <- 5x
  ;; [0 3 4]     -> 25
  ;; [0 1 2 4 5] -> 55     <- 5x
  ;; [1 2]       -> 6      <-

  ;; 4x [31 55], 2x [6], [25 55]
  
  ;; 55
  ;; 6
  ;; 31
  (halving [8 8 8 4 8 4]);; [4 [2 2 2 1 2 1]]
  (halving [2 2 2 1 2 1]);; [1 [2 2 2 1 2 1]]
  (halving [4 2 4 8 6 4]);; [2 [2 1 2 4 3 2]]

  ;; --------------------------------------------------------------------------
  ;; try some real lines of input
  (process-one-pt2 "[#.#..#] (1,3) (3,4) (0,3,5) (1,2,3,4) (0,2,5) (0,1) (2,5) {44,35,48,43,24,44}")
  (process-one-pt2 "[#.####.] (2,3,4,6) (2,5) (0,1,2,4,5,6) (0,1,2,4,5) (0,2,3,4,5) (0,1,3,5,6) (5,6) (0,3,4,5,6) {61,52,67,37,57,87,63}") ;; 99

  (runner ["[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
           "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
           "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"]) ;; 7

  (runner2 ["[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"
            "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}"
            "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"]) ;; 33

  (with-open [r (io/reader (io/resource "aoc-2025/day10.txt"))]
    (runner (line-seq r))) ;; 375

  (time (with-open [r (io/reader (io/resource "aoc-2025/day10.txt"))]
     (runner2 (line-seq r))));; 15182 too low

  

  ;; end comment
  )
