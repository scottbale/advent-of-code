(ns aoc-2025.day2
  "docstring"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as s]))

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

(def invalid-N-id?
  "Returns a function that checks whether an id has a pattern repeated N times? A
  generalization of `invalid-id?` from 2 to N."
  (memoize
   (fn [n]
     (fn [id]
       (let [id-str (str id)
             c (count id-str)]
         (when (= 0 (mod c n))
           (let [l (/ c n)]
             (apply = (partition l id-str)))))))))

(defn invalid-id-2?
  "Part 2: an id is invalid if is composed of any number repeated _at least_
  twice. Takes a number input."
  [id]
  (let [c (count (str id))]
    (when (<= 2 c)
      (let [ns (range 2 (inc c))
            fns (map invalid-N-id? ns)]
        ((apply some-fn fns) id)))))

(defn runner-helper
  [pred input]
  (let [id-ranges (s/split input #"\,")
        ids (mapcat id-range-str->range id-ranges)
        invalid-ids (filter pred ids)]
    (reduce + invalid-ids)))

(defn runner [input]
  (runner-helper invalid-id? input))

(defn runner2 [input]
  (runner-helper invalid-id-2? input))


(comment

  ((invalid-N-id? 3) 212121)
  ((invalid-N-id? 3) 5150)
  ((invalid-N-id? 3) 212112)
  (invalid-id-2? 5150)
  (invalid-id-2? 5151)
  (invalid-id-2? 1111111)
  (invalid-id-2? 1111311)

  (invalid-id-str? "2112")

  (id-range-str->range "11-15")
  
  (runner "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124") ;; 1227775554

  (runner2 "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124") ;; 4174379265

  (runner (slurp (io/resource "aoc-2025/day2.txt"))) ;; 18595663903
  (runner2 (slurp (io/resource "aoc-2025/day2.txt")));; 19058204438

  ;; end comment
  )
