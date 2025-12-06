(ns aoc-2025.day1
  "Secret entrance password"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [debugger :refer [dbg]]))

(def turns->fns {\L -
                 \R +})

(defn runner
  "Given the input sequence of safe dial turns, returns the number of times the
  dial points at zero after any turn. The dial starts by pointing at 50. Zero to
  99 are the dial numbers."
  [input]
  (->> input
       ;; Parse input into a sequence of pairs: a function (plus or minus) and a number
       (map (fn [[direction & digits]]
              [(turns->fns direction) (edn/read-string (apply str digits))]))
       ;; Starting with a list of "50", grow the list by adding each of the next
       ;; dial values after each turn. Just let the values overflow 99 or less
       ;; than 0.
       (reduce 
        (fn [[n :as ns] [f delta]]
          (conj ns (f n delta))) 
        (list 50))
       ;; do a mod operation so all of the multiples of 100 become "zero"
       (map (fn [n] (mod n 100)))
       ;; filter and count only the zeroes
       (filter (fn [n] (= 0 n)))
       count))

(defn ceil-100
  "Returns the closest multiple of 100 that is greater or equal to n."
  [n]
  (- (+ n 100) (mod n 100)))

(defn floor-100
  "Returns the closest multiple of 100 that is less than or equal to n."
  [n]
  (- n (mod n 100)))

(defn zero-counts
  "Why is this so difficult? Curse my feeble brain! Given two ints on the number
  line, return the number of multiples of 100 (`n`-inclusive) in their range."
  [m n]
  (cond
    (> n m)
    (let [m-ceil (ceil-100 m)]
      (if (<= m-ceil n)
        (inc (long (/ (- n m-ceil) 100)))
        0))
    (< n m)
    (let [m-floor (floor-100 m)]
      (if (<= n m-floor)
        (let [result (long (/ (- m-floor n) 100))]
          (if (= m m-floor)
            result
            (inc result)))
        0))
    :else 0))

(defn runner2
  "Given the input sequence of safe dial turns, returns the number of times the
  dial passes or points at zero."
  [input]
  (->> input
       ;; Parse input into a sequence of pairs: a function (plus or minus) and a number
       (map (fn [[direction & digits]]
              [(turns->fns direction) (edn/read-string (apply str digits))]))
       ;; Starting with a list of "50", grow the list by adding each of the next
       ;; dial values after each turn. Just let the values overflow 99 or less
       ;; than 0.
       (reduce
        (fn [[n :as ns] [f delta]]
          (conj ns (f n delta)))
        (list 50))

       reverse
       vec

       ;; pairs
       (reduce
        (fn [m n]
          (if (vector? m)
            (let [[_ b'] (last m)]
              (conj m [b' n]))
            [[m n]])))

       (reduce
        (fn [counts [m n]]
          (+ counts (zero-counts m n)))
        0)

       ;; dbg
       #_(reduce
        (fn [triples [m n]]
          (conj triples [m n (zero-counts m n)]))
        [])

       ;; Let us never speak of this again.
       ))

(comment

  (require ['clojure.test :refer ['deftest 'is]])
  (deftest zero-counts-tests
    (doseq [[a b expected] [[0 1 0]
                            [1 0 1]
                            [1 2 0]
                            [99 100 1]
                            [99 101 1]
                            [-101 -99 1]
                            [-102 -101 0]
                            [50 -18 1]
                            [-18 -48 0]
                            [-48 0 1]
                            [0 3 0]
                            [3 0 1]
                            [0 -5 0]
                            [203 200 1]
                            [-48 225 3]
                            [225 -48 3]]]
      (is (= expected (zero-counts a b))
          (format "%d and %d did not yield %d" a b expected))))
  (zero-counts-tests)

  ;; should return 3
  (runner ["L68"
           "L30"
           "R48"
           "L5"
           "R60"
           "L55"
           "L1"
           "L99"
           "R14"
           "L82"]) ;; 3

  ;; should return 6
  (runner2 ["L68"
            "L30"
            "R48"
            "L5"
            "R60"
            "L55"
            "L1"
            "L99"
            "R14"
            "L82"]) ;; 6
  #_  [[50 -18 1]
       [-18 -48 0]
       [-48 0 1]
       [0 -5 0]
       [-5 55 1]
       [55 0 1]
       [0 -1 0]
       [-1 -100 1]
       [-100 -86 0]
       [-86 -168 1]]

  (with-open [r (io/reader (io/resource "aoc-2025/day1.txt"))]
    (runner (line-seq r))) ;; 1120

  (with-open [r (io/reader (io/resource "aoc-2025/day1.txt"))]
    (runner2 (line-seq r))) ;; 6554

  ;; end comment
  )


