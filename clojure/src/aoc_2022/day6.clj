(ns aoc-2022.day6
  "Tuning trouble"
  (:require
   [clojure.java.io :as io]))

(defn find-marker
  "Recursively traverse input, with a FIFO queue of most recent `n` chars. At each step: if queue has
  `n` unique chars then success, return `i`. Otherwise, recurse, incrementing `i`, popping next val
  off input and into `fifo`, and dropping oldest `fifo` val."
  [n i fifo input]
  (if (= n (count (set fifo)))
    i
    (recur n (inc i) (-> fifo pop (conj (first input))) (rest input))))

(defn runner
  "Initialize a FIFO queue from the first `n` input vals. Starting with that, recursively search for
  the packet marker."
  [n input]
  (let [input (first input)
        input' (drop n input)
        fifo (apply conj clojure.lang.PersistentQueue/EMPTY (take n input))]
     (find-marker n n fifo input')))


(comment

  ;; pt 1
  (runner 4 ["mjqjpqmgbljsphdztnvjfqwrcgsmlb"]) ;; 7
  (runner 4 ["bvwbjplbgvbhsrlpgdmjqwftvncz"]) ;; 5
  (runner 4 ["nppdvjthqldpwncqszvftbrmjlhg"]) ;; 6
  (runner 4 ["nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"]) ;; 10

  (with-open [r (io/reader (io/resource "aoc-2022/day6.txt"))]
    (runner 4 (line-seq r))) ;; 1134

  ;; pt 2
  (runner 14 ["mjqjpqmgbljsphdztnvjfqwrcgsmlb"]) ;; 19
  (runner 14 ["bvwbjplbgvbhsrlpgdmjqwftvncz"]) ;; 23
  (runner 14 ["nppdvjthqldpwncqszvftbrmjlhg"]) ;; 23
  (runner 14 ["nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"]) ;; 29

  (with-open [r (io/reader (io/resource "aoc-2022/day6.txt"))]
    (runner 14 (line-seq r))) ;; 2263

  ;; expect 12?
  (let [fifo clojure.lang.PersistentQueue/EMPTY
        fifo (conj fifo \a \b \a \c)
        input "abaadabcfoobar"]
    (find-marker 4 4 fifo input))

  (into (list) (conj (pop (conj clojure.lang.PersistentQueue/EMPTY 1 2 3)) 4 5))
  (set (conj (pop (conj clojure.lang.PersistentQueue/EMPTY 1 2 3)) 4 5))


  )
