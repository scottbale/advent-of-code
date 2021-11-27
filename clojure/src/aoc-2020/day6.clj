(ns aoc-2020.day6
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [debugger :refer [dbg]]))

(defn group-seqs
  "Given single input sequence, return sequence of collections each representing a single group's
  responses"
  [input]
  (->> input
       (partition-by empty?)
       (remove (comp empty? first))))

(defn tally-questions-answered*
  [keyset-fn input]
  (->> input
       (map (comp set keys frequencies))
       (apply keyset-fn)
       count))

(def tally-questions-answered-by-any
  "Given group input (sequence of one or more person's answers), return count of number of unique
  questions answered by anyone in the group"
  (partial tally-questions-answered* set/union))

(def tally-questions-answered-by-every
  "Given group input (sequence of one or more person's answers), return count of number of unique
  questions for which every person in the group supplied an answer"
  (partial tally-questions-answered* set/intersection))

(defn runner [input]
  (->> input
       group-seqs
       (map tally-questions-answered-by-every)
       (reduce + 0)))

(comment



  (let [map-1 {\a 1 \b 6}
        map-2 {\a 2 \c 3}]
    (set/union (set (keys map-1)) (set (keys map-2))))

  (frequencies "aabcc")

  (tally-questions-answered-by-any 
   ["abc"
    "a"
    "b"
    "c"
    "ab"
    "ac"
    "a"
    "a"
    "a"
    "a"
    "b"])
  (tally-questions-answered-by-every 
   ["abc"
    "a"
    "ab"
    "ac"
    "a"
    "a"
    "a"
    "a"
    "ba"])

  (group-seqs ["a" "aa" "" "ab" "" "b" "b" "" "abc"])

  (runner ["abc"
           ""
           "a"
           "b"
           "c"
           ""
           "ab"
           "ac"
           ""
           "a"
           "a"
           "a"
           "a"
           ""
           "b"])

  (with-open [r (io/reader (io/resource "aoc-2020/day6.txt"))]
    (runner (line-seq r)))

  )
