(ns aoc-2025.day7
  "Laboratories, tachyon beams and splitters"
  (:require
   [clojure.java.io :as io]
   [debugger :refer [dbg]]))

(defn step
  "Reducing step function. Given the previous state and the next line of
  input (which contains zero or more splitters), return an updated state which
  is the index of tachyons on that line and the updated total count of splits."
  [{:keys [tachyons] :as m} splitters]
  (let [splitset (set splitters)]
    (reduce
     (fn [{:keys [split-count tachyons]} t]
       (if (splitset t)
         {:split-count (inc split-count)
          :tachyons (conj tachyons (dec t) (inc t))}
         {:split-count split-count
          :tachyons (conj tachyons t)}))
     (assoc m :tachyons #{})
     tachyons)))

(defn input->indices
  "Given a string line of input and a set of chars, return a coll of indices of
  any of the chars in the input line."
  [chars s]
  (let [alist (map vector s (range))]
    (->>
     alist
     (filter (fn [[x _i]]
               (chars x)))
     (map second)
     set)))

(defn runner
  "Reduce over the lines of input. Calculate the indices of tachyons at each
  successive line, and tally the total number of times the tachyon beam split."
  [[in & ins :as _input]]
  (let [indices-f (partial input->indices #{\S \| \^})]
    (reduce
     (fn [m k]
       (step m (indices-f k)))
     {:tachyons (indices-f in)
      :split-count 0}
     ins)))

(comment

  (input->indices #{\S \| \^} "......S......")
  (input->indices #{\S \| \^} "...|..S......")
  (input->indices #{\S \| \^} "...^..S.^....")

  (step {:tachyons #{1 7}
         :split-count 0} #{})
  (step {:tachyons #{1 7}
         :split-count 0} #{2 4})
  (step {:tachyons #{1 7}
         :split-count 0} #{2 7 9})
  (step {:tachyons #{1 3 4 5 7 8 10 11 13}
         :split-count 10} #{1 3 5 7 9 13})

  (runner [".......S......."
           "..............."
           ".......^......."
           "..............."
           "......^.^......"
           "..............."
           ".....^.^.^....."
           "..............."
           "....^.^...^...."
           "..............."
           "...^.^...^.^..."
           "..............."
           "..^...^.....^.."
           "..............."
           ".^.^.^.^.^...^."
           "..............."]);; {:split-count 21, :tachyons #{0 4 6 12 2 11 14 10 8}}

  (with-open [r (io/reader (io/resource "aoc-2025/day7.txt"))]
    (runner (line-seq r))) ;; {:split-count 1628, :tachyons #{0 70 62 74 110 86 20 72 58 60 101 102 135 88 46 4 106 54 92 137 104 48 50 116 99 113 32 40 129 91 117 108 56 22 90 36 118 100 131 122 44 6 28 134 64 51 25 34 12 2 66 23 127 82 76 97 68 112 138 14 45 78 132 26 140 16 120 38 126 124 30 96 10 18 52 114 67 42 80 94 8 84}}

  ;; end comment
  )
