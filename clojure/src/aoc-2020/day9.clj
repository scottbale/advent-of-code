(ns aoc-2020.day9
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [debugger :refer [dbg]]))



(defn legit? [prev-N number]
  (contains? (set (for [x prev-N y prev-N :when (< y x)] (+ x y))) number))

(defn find-range-summing-to
  "Search the input, finding a range of contiguous values whose sum is equal to n."
  [n input]
  (letfn [(slide [[nm & more :as sliding-input] sum test-range]
            (let [k (+ nm sum)
                  test-range (conj test-range nm)]
              (if (< k n)
                (recur more k test-range)
                [k test-range])))
          (step [sliding-input]
            (when (seq sliding-input)
              (let [[sum test-range] (slide sliding-input 0 [])]
                (if (== sum n)
                  test-range
                  (recur (rest sliding-input))))))]
    (step input)))

(defn runner [preamble input]
  (letfn [(step [sliding-input]
            (when (seq sliding-input)
              (let [window (take preamble sliding-input)
                    nm (->> sliding-input (drop preamble) first)]
                (if (legit? window nm)
                  (recur (rest sliding-input))
                  nm))))]
    (let [invalid-nm (step input)
          summing-range (find-range-summing-to invalid-nm input)]
      (+ (apply min summing-range) (apply max summing-range)))))

(comment

  (def test-input [35
                   20
                   15
                   25
                   47
                   40
                   62
                   55
                   65
                   95
                   102
                   117
                   150
                   182
                   127
                   219
                   299
                   277
                   309
                   576])

  (legit? (take 5 test-input) 62)

  (with-open [r (io/reader (io/resource "aoc-2020/day9.txt"))]
    (let [input (map edn/read-string (take 26 (line-seq r)))]
      (legit? (take 25 input) (last input))))

  (runner 5 test-input)   ;; 127  ;; 62
  (find-range-summing-to 127 test-input) ;;[15 25 47 40]

  (with-open [r (io/reader (io/resource "aoc-2020/day9.txt"))]
    (runner 25 (map edn/read-string (line-seq r)))) ;;756008079  ;; 93727241




  )
