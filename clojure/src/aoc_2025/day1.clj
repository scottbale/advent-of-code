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
       count
       )
  )


(comment

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

  (with-open [r (io/reader (io/resource "aoc-2025/day1.txt"))]
    (runner (line-seq r)))  ;; 1120

  ;; end comment
  )


