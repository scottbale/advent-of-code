(ns aoc-2025.day2
  "docstring"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as s]
   [debugger :refer [dbg]]))

(defn invalid-id-str?
  "An id is invalid if it is composed of a single number repeated twice. So, an
  invalid id's length is an even number, can be split exactly in two, and the
  two halves must be equal."
  [id-str]
  (let [c (count id-str)]
    (when (even? c)
      (let [half-c (bit-shift-right c 1)
            first-half (take half-c id-str)
            second-half (drop half-c id-str)]
        (= first-half second-half)))))

(defn invalid-id?
  "An id is invalid if it is composed of a single number repeated twice. So, an
  invalid id's length is an even number, can be split exactly in two, and the
  two halves must be equal."
  [id]
  (invalid-id-str? (str id)))

(defn id-range-str->range
  "Given a string like '11-22', return a lazy seq range of numbers inclusive"
  [range-str]
  (let [[a b] (s/split range-str #"\-")
        a (edn/read-string a)
        b (edn/read-string b)]
    (range a (inc b))))

(defn runner
  "runner docstring"
  [input]
  (let [id-ranges (s/split input #"\,")
        ids (mapcat id-range-str->range id-ranges)
        invalid-ids (filter invalid-id? ids)]
    (reduce + invalid-ids)))


(comment

  (invalid-id-str? "2112")

  (id-range-str->range "11-15")
  
  (runner "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124") ;; 1227775554

  (runner (slurp (io/resource "aoc-2025/day2.txt"))) ;; 18595663903

  ;; end comment
  )
