(ns aoc-2020.day5
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [debugger :refer [dbg]]))

;; B,R -> 1
;; F,L -> 0
(def decode {\F \0
             \L \0
             \B \1
             \R \1})

(defn decode-number [s]
  (->> s
       (map decode)
       (apply str "2r")
       edn/read-string))

(defn runner [input]
  (apply max (map decode-number input)))

(defn deduce-missing
  [[i result] j]
  (cond
    (not (nil? result)) [nil result]
    (nil? i) [j result]
    (= (inc i) j) [j result]
    :found-the-answer [nil (inc i)] ))

(defn find-missing [input]
  (let [[_ result] (reduce deduce-missing [nil nil] input)]
    result))

#_(defn runner2 [input]
  (find-missing (sort (map decode-number input))))

#_(defn runner2 [input]
  (->> input
       (map decode-number)
       sort
       find-missing))

;; just showing off
(def runner2 (comp find-missing sort (partial map decode-number)))

(comment

  (deduce-missing [nil nil] 1)
  (deduce-missing [2 nil] 3)
  (deduce-missing [4 2] 5)
  (deduce-missing [2 nil] 4)

  (decode-number "BFFFBBFRRR")

  (find-missing [3 4 5 6 8 9 10 11])

  (runner2 ["BFFFBBFRRR"
            "FFFBBBFRRR"
            "BBFFBBFRLL"])

  (with-open [r (io/reader (io/resource "aoc-2020/day5.txt"))]
    (runner2 (line-seq r))) ;; 659     ;;896


  ;; For example, consider just the first seven characters of FBFBBFFRLR:

  ;; Start by considering the whole range, rows 0 through 127.

  ;; 000 0000 to 111 1111
  ;; FBF BBFF    FBF BBFF

  ;; F means to take the lower half, keeping rows 0 through 63.

  ;; 000 0000 0 to 011 1111 63
  ;; 100 0000 64 and up

  ;; B means to take the upper half, keeping rows 32 through 63.

  ;; 001 1111 up to 31
  ;; 010 0000 32 to 011 1111 63

  ;; F means to take the lower half, keeping rows 32 through 47.

  ;; 010 0000 32 to 010 1111 47

  ;; B means to take the upper half, keeping rows 40 through 47.

  ;; 010 1000 40 to 010 1111 47

  ;; B keeps rows 44 through 47.

  ;; 010 1100 44 to 010 1111 47

  ;; F keeps rows 44 through 45.

  ;; 010 1100 44 to 010 1101 45

  ;; The final F keeps the lower of the two, row 44.

  ;; 010 1100 44 to 010 1100 44


  ;; F flips a bit from 1 to 0 in the range upper bound
  ;; U flips a bit from 0 to 1 in the range lower bound


  ;; 000 0000 to 111 1111
  ;; FBF BBFF    FBF BBFF
  ;; 010 1100    010 1100
  ;; or          and
  ;; 010 1100    010 1100

  ;; B,R -> 1
  ;; F,L -> 0

  ;; BFFFBBFRRR 567
  ;; 1000110111


  ;; FFFBBBFRRR 119
  ;; 0001110111

  ;; BBFFBBFRLL 820
  ;; 1100110100
  )
