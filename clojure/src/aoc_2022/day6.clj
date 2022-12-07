(ns aoc-2022.day6
  "Tuning trouble"
  (:require
   [clojure.java.io :as io]))

(defn find-marker
  "Recursively traverse input, with a FIFO queue of most recent 4 chars. At each step: if queue has
  four unique chars then success, return `n`. Otherwise, recurse, incrementing `n`, popping next val
  off input and into `fifo`, and dropping oldest `fifo` val."
  [n fifo input]
  (if (= 4 (count (set fifo)))
    n
    (recur (inc n) (-> fifo pop (conj (first input))) (rest input))))

(defn runner
  "Initialize a FIFO queue from the first four input vals. Starting with that, recursively search for
  the start-of-packet marker."
  [input]
  (let [input (first input)
        n 4
        input' (drop n input)
        fifo (apply conj clojure.lang.PersistentQueue/EMPTY (take n input))]
     (find-marker n fifo input')))


(comment

  (runner ["mjqjpqmgbljsphdztnvjfqwrcgsmlb"]) ;; 7
  (runner ["bvwbjplbgvbhsrlpgdmjqwftvncz"]) ;; 5
  (runner ["nppdvjthqldpwncqszvftbrmjlhg"]) ;; 6
  (runner ["nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"]) ;; 10

  (with-open [r (io/reader (io/resource "aoc-2022/day6.txt"))]
    (runner (line-seq r))) ;; 1134

  ;; expect 12?
  (let [fifo clojure.lang.PersistentQueue/EMPTY
        fifo (conj fifo \a \b \a \c)
        input "abaadabcfoobar"]
    (find-marker 4 fifo input)) 

  (into (list) (conj (pop (conj clojure.lang.PersistentQueue/EMPTY 1 2 3)) 4 5))
  (set (conj (pop (conj clojure.lang.PersistentQueue/EMPTY 1 2 3)) 4 5))


  )
