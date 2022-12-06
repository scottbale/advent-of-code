(ns aoc-2022.day3
  "rucksack partitions"
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]))

(def priorities 
  "Each item (represented by a char) is assigned a numeric priority
   a-z -> 1-26, A-Z -> 27-52"
  (zipmap
   (concat (map char (range 97 (inc 122))) (map char (range 65 (inc 90))))
   (range 1 53)))

(defn priority
  "Given a single line of input: find the char present in both halves, return its priority."
  [input]
  (let [[p1 p2] (partition (/ (count input) 2) input)
        s1 (set p1)
        s2 (set p2)
        item (first (set/intersection s1 s2))]
    (priorities item)))

(defn runner1
  "Each line of input is a string representing the contents of two equally-sized partitions. The
  partitions are supposed to have unique items, but each input pair of partitions contains exactly
  one duplicate char."
  [input]
  (reduce + (map priority input)))

(defn runner
  "Every three lines of input is an elf group. Exactly one item (char) will be found in all three
  rucksacks. Find the sum of the priorities of each of those items."
  [input]
  (let [elf-groups (partition 3 input)]
    (letfn [(find-item [ps]
              (let [ss (map set ps)]
                (->> ss (apply set/intersection) first)))]
      (reduce + (map (comp priorities find-item) elf-groups)))))

(comment

  (runner ["vJrwpWtwJgWrhcsFMMfFFhFp"
           "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
           "PmmdzqPrVvPwwTWBwg"
           "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
           "ttgJtRGJQctTZtZT"
           "CrZsJsPPZsGzwwsLwLmpwMDw"]) ;; 70 ;; 157

  (with-open [r (io/reader (io/resource "aoc-2022/day3.txt"))]
    (runner (line-seq r))) ;; 2488 ;; 7863

  (priority "vJrwpWtwJgWrhcsFMMfFFhFp")

  )
