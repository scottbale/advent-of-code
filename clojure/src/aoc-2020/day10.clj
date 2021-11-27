(ns aoc-2020.day10
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [debugger :refer [dbg]]))

(defn gen-pascals-triangle-data
  "Generate a sequence of the sum of the first 3 values of (optionally) the first 'n' (one-based) rows
  of Pascal's triangle. Lazy. To be clear, Pascal's triangle rows are ordinarily zero-based, but
  this function is written with 'n' being one-based, i.e. n=1 refers to zeroth row. So parameter of
  n=0 will result in an empty sequence being returned (same as taking zero from a sequence)."
  ([]
   (letfn [(gen-row [previous-row]
             (map + previous-row (concat [0] previous-row)))
           (step [previous-row] ;; generate current row, return sum of numbers
             (let [current-row (gen-row previous-row)
                   current-sum (reduce + 0 current-row)]
               (concat [current-sum] (lazy-seq (step current-row)))))]
     (let [first-three-sums [1 2 4]
           third-row [1 2 1]]
       (concat first-three-sums (step third-row)))))
  ([n]
   (case n
     1 [1]
     2 [1 2]
     3 [1 2 4]
     (take n (gen-pascals-triangle-data)))))

(defn diff-seq
  "For the given sorted seq of numbers, return a seq of differences of adjacent numbers."
  [sorted-input]
  (map - sorted-input (concat [0] sorted-input)))

(defn runner [input]
  (let [pascals (gen-pascals-triangle-data) ;; caution! infinite! do not attempt to print out
        pascals-nth (comp (partial nth pascals) dec) ;; adjust from 1-based to 0-based
        ]
    (->> input
         sort
         diff-seq
         (partition-by identity) ;; get the sequences of contiguous ones and threes
         (take-nth 2) ;; retain only the sequences of contiguous ones
         (map (comp pascals-nth count)) ;; number combinations of each block of contiguous ones is
                                        ;; gotten by looking up the count of each block of
                                        ;; contiguous ones in the Pascal sequence
         (reduce * 1) ;; take the product of all the combinations
         )))

(defn debug [input]
  (let [pascals (gen-pascals-triangle-data) ;; caution! infinite! do not attempt to print out
        pascals-nth (comp (partial nth pascals) dec) ;; adjust from 1-based to 0-based
        input (sort input)
        diffs (-> input diff-seq)
        diff-partitions (->> diffs (partition-by identity) (take-nth 2))
        diff-sums (->> diff-partitions (map count))
        diff-sum-pascal-vals (map pascals-nth diff-sums)
        product (reduce * 1 diff-sum-pascal-vals)
        debug {:input input
               :diffs diffs
               :diff-partitions diff-partitions
               :diff-sums diff-sums
               :diff-sum-pascal-vals diff-sum-pascal-vals
               :product product}]
    debug))



(comment


  (with-open [r (io/reader (io/resource "aoc-2020/day10.txt"))]
    (runner (map edn/read-string (line-seq r)))) ;; 43,406,276,662,336
  ;; 1,190,895,478,117,043,776 too high

  (runner [16
           10
           15
           5
           1
           11
           7
           19
           6
           12
           4]) ;; 8

  (runner [28
           33
           18
           42
           31
           14
           46
           20
           48
           47
           24
           23
           49
           45
           19
           38
           39
           11
           1
           32
           25
           35
           8
           17
           7
           9
           4
           2
           34
           10
           3]) ;; 19208

  ;; Below is me figuring out how number of combinations of contiguous 1's could be calculated using Pascal's triangle

  ;;    n = 3  three ones
  ;; f(n) = 4  four combinations

  [4 5 6 7 10]
  [3 1 1 1 3]

  [4 5 7 10]
  [3 1 2 3]

  [4 6 7 10]
  [3 2 1 3]

  [4 7 10]
  [3 3 3]

  

  ;;    n = 4  four ones
  ;; f(n) = 7  seven combinations

  [1 2 3 4 7] 
  [1 1 1 1 3]
  
  [1 3 4 7] 
  [1 2 1 3]
  
  [1 2 4 7] 
  [1 1 2 3]

  [2 3 4 7] 
  [2 1 1 3]

  [1 4 7] 
  [1 3 3]
  
  [2 4 7] 
  [2 2 3]
  
  [3 4 7] 
  [3 1 3]

  ;;    n = 5  five ones
  ;; f(n) =    combinations

  [1 2 3 4 5 8] 
  [1 1 1 1 1 3]
  
  [1 2 3 5 8] 
  [1 1 1 2 3]
  
  [1 2 4 5 8] 
  [1 1 2 1 3]
  
  [1 3 4 5 8] 
  [1 2 1 1 3]
  
  [2 3 4 5 8] 
  [2 1 1 1 3]
  
  [1 2 5 8] 
  [1 1 3 3]
  
  [1 3 5 8] 
  [1 2 2 3]
  
  [1 4 5 8] 
  [1 3 1 3]
  
  [2 4 5 8] 
  [2 2 1 3]
  
  [2 3 5 8] 
  [2 1 2 3]
  
  [3 4 5 8] 
  [3 1 1 3]
  

  ; n | f(n)
  ; --|-----
  ; 1 | 1
  ; 2 | 2    = 1 + 1
  ; 3 | 4    = 1 + 2 + 1   
  ; 4 | 7    = 1 + 3 + 3
  ; 5 | 11   = 1 + 4 + 6

  ; n = number of contiguous 1's in sequence of jolt diffs
  ; f(n) = sum of first three numbers of row (n-1) of Pascal's triangle (which has zero-based rows)
  
  )
