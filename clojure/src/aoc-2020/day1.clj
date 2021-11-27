(ns aoc-2020.day1
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [debugger :refer [dbg]]))

(defn find-sum-pair*
  [target [s :as sorted-input] [r :as reversed-input]]
  ;; Problem: given an input, find the two numbers whose sum is 2020
  ;; 
  ;; Naive approach is to check every sum, which is O(n^2)
  ;; 
  ;; Instead
  ;; * sort the list least to greatest - O(n log n) assuming java.util.Arrays/sort default of QuickSort
  ;; * reverse the list - O(n)
  ;; * traverse each list one time:
  ;; * for list1 and list2 (list2 being the reversed)
  ;;   * sum heads of each list
  ;;     * if sum == 2020, answer is found
  ;;     * if sum > 2020
  ;;       then recurse with list1 (rest list2) because there are no elements that can be summed with (head list2) to equal 2020
  ;;     * keep checking list2 while sum < 2020
  ;; (println ">>>>>step" s r)
  (if (and s r (<= s r))
    (let [reversed-input (drop-while (fn [i] (> (+ s i) target)) reversed-input)
          r' (first (take-while (fn [i] (and (<= (+ s i) target) (>= i s))) reversed-input))]
      ;;(println ">>>>>>checking r'" r')
      (if (and r' (== target (+ s r')))
        [s r']
        (recur target (rest sorted-input) reversed-input)))))

(defn find-sum-pair
  "Given the target and an input sequence of numbers, find and return the pair of numbers whose sum
  equals the target"
  [target input]
  (let [sorted-input (sort input)
        reversed-input (reverse sorted-input)]
    ;; (println "min" (first sorted-input) "max" (first reversed-input))
    (find-sum-pair* target sorted-input reversed-input)))

(defn find-sum-triple
  "Given the target and an input sequence of numbers, find and return the three numbers whose sum
  equals the target"
  [target input]
  (let [sorted-input (sort input)
        reversed-input (reverse sorted-input)]
    ;; (println "min" (first sorted-input) "max" (first reversed-input))
    (letfn [(step [[x :as xs]]
              ;; (println ">>>>>triple step" x)
              (if-let [x (first xs)]
                (if-let [pair (find-sum-pair* (- target x) sorted-input (rest xs))]
                  (conj pair x)
                  (recur (rest xs)))))]
      (step reversed-input))))

(defn find-sum-pair-naive
  "Given the target and an input sequence of numbers, find and return the pair of numbers whose sum
  equals the target"
  [target input]
  (letfn [(inner [i il2]
            (let [il2 (drop-while (fn [j] (not (== (+ i j) target))) il2)]
              (if-let [j (first il2)]
                [i j])))
          (outer [i1 i2]
            (if (seq i1)
              (if-let [x (inner (first i1) i2)]
                x
                (recur (rest i1) i2))))]
    (outer input input)))

(comment


  (def input [1721
              979
              366
              299
              675
              1456])

  (find-sum-pair 2020 input)
  (find-sum-pair-naive 2020 input)
  (find-sum-triple 2020 input) ;; [366 675 979]

  (time (with-open [r (io/reader (io/resource "aoc-2020/day1.txt"))]
          ;;(find-sum-pair 2020 (map edn/read-string (line-seq r)))
          ;;(find-sum-pair-naive 2020 (map edn/read-string (line-seq r)))
          (find-sum-triple 2020 (map edn/read-string (line-seq r)))
          )) ;[512 513 995]



  ;; (299 366 675 979 1456 1721)

  ;; (1721 1456 979 675 366 299)


  )
