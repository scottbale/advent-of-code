(ns aoc-2023.day1
  "Trebuchet. Parse two-digit numbers from each input line of text. Part two has some numbers spelled
  out as words."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [the.parsatron :as p]))

(def number-chars #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(defn input-to-number
  "Given a string, filter only the chars that are digits (only first and last) and turn those into an
  n-digit long"
  [input-str]
  (letfn [(first-and-last [[x & xs]]
            [x (or (last xs) x)])]
    (->>
     input-str
     (filter number-chars)
     first-and-last
     (apply str)
     (edn/read-string))))

(defn input-to-first-number
  "Given a string (or seq of chars), return the first number char"
  [input-char-seq]
  (->>
   input-char-seq
   (filter number-chars)
   first))

(defn runner
  "runner for part 1."
  [input]
  (->> input (map input-to-number) (reduce +)))

(defn build-number-word-parser [[k v]]
  (p/attempt (p/nxt (p/string k) (p/always v))))

(def number-words 
  {;; forwards ...
   "one" \1
   "two" \2
   "three" \3
   "four" \4
   "five" \5
   "six" \6
   "seven" \7
   "eight" \8
   "nine" \9
   ;; ... and backwards
   "eno" \1
   "owt" \2
   "eerht" \3
   "ruof" \4
   "evif" \5
   "xis" \6
   "neves" \7
   "thgie" \8
   "enin" \9
   })

;; Transform the input: replace spelled-out numbers with a single digit char
(p/defparser xform []
  (let [parsers (mapv build-number-word-parser number-words)]
    (p/many1 
     (apply p/choice (conj parsers (p/letter) (p/digit))))))

(defn do-one-input
  "Given a single input, return the long number it represents. 
  Parse the input in the forward and then reverse direction,
  taking the first digit in each case, to form the two-digit number."
  [parser input]
  (let [first-digit (->> input (p/run parser) input-to-first-number)
        last-digit (->> input reverse (p/run parser) input-to-first-number)]
    (edn/read-string (str first-digit last-digit))))

(defn runner2
  "runner for part 2."
  [input]
  (let [parser (xform)]
    (->> input (map (partial do-one-input parser)) (reduce +))))


(comment

  (p/defparser parse-one []
    (p/>> (p/string "one") (p/always 1)))

  (p/defparser parse-one-inclusive []
    (p/nxt (p/many (p/letter)) (parse-one)))

  (p/run (parse-one) "one")
  (p/run (parse-one) "two")
  (p/run (build-number-word-parser ["one" 1]) "one")
  (p/run (build-number-word-parser ["one" 1]) "two")
  (p/run (parse-one) "onetwothree")
  (p/run (parse-one-inclusive) "xonetwothree")
  (p/run (p/many (p/letter)) "xonetwothree")
  (p/run (p/many1 (p/either (parse-one) (p/letter))) "xone")
  (p/run (p/many1 (p/either (parse-one) (p/letter))) "xoney")
  (p/run (p/many1 (p/either (parse-one) (p/letter))) "xoneyzabzo")
  (p/run (p/many1 (p/either (p/attempt (parse-one)) (p/letter))) "xonetwo")
  (p/run (p/many1 (p/either (p/attempt (parse-one)) (p/letter))) "xonetwothree")

  (p/run (xform) "eightwothree")

;; part 1

  (runner ["1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet"]) ;; 142

  (with-open [r (io/reader (io/resource "aoc-2023/day1.txt"))]
    (runner (line-seq r))) ;; 54630

  ;; part 2

  (runner2 ["1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet"]) ;; 142
  (runner2 ["two1nine" "eightwothree" "abcone2threexyz" "xtwone3four" "4nineeightseven2" "zoneight234" "7pqrstsixteen"]) ;; 281

  (with-open [r (io/reader (io/resource "aoc-2023/day1.txt"))]
    (runner2 (line-seq r))) ;; 54770 ;; 54780 too high ;; 54630

  (p/run (xform) "ninesevensrzxkzpmgz8kcjxsbdftwoner")

  ;; This is an example of a tricky input because of the end of the string "twone" - the 
  ;; final number should be "1" but when I parsed in a forward direction I got "2" and
  ;; didn't recognize the trailing one.
  (do-one-input (xform) "ninesevensrzxkzpmgz8kcjxsbdftwoner") ;; 91

  (input-to-number "ab3d6e7g")
  (input-to-number "treb7uchet")

  (first-number "abcone2threexyz"))
