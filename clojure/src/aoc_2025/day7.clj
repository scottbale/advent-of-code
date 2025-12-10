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

(defn step2
  "The part 2 step function differs from part 1: the input is (1) a _coll_ of
  previous states (representing each of the 'timelines'), initially of size 1;
  and (2) the same `splitters` as before which represents the next line of
  input. The output is a _collection_ of next states. If any splits occurred,
  the size of the output collection will be larger than the size of the
  input (up to twice as large). The state is simplified from part 1: no need to
  track `:split-count`, and each state now only has a single tachyon number
  index, so the state is simply a single number. (Important to retain
  duplicates, because duplicates represent different timelines that arrive at
  the same index.)"
  [states splitters]
  (reduce
   (fn [new-states timeline-index]
     (if (splitters timeline-index)
       (conj new-states (dec timeline-index) (inc timeline-index))
       (conj new-states timeline-index)))
   []
   states))

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

(defn runner2
  "Reduce over the lines of input. Calculate the indices of tachyons at each
  successive line, and tally the total number of unique timelines (which is
  equivalent to the size of the timeline index collection after the final step)"
  [[in & ins :as _input]]
  (let [indices-f (partial input->indices #{\S \| \^})]
    (count (reduce
      (fn [m k]
        (step2 m (indices-f k)))
      (vec (indices-f in))
      ins))))

(comment

  (step2 [1 7]
         #{})
  (step2 [1 7]
         #{2 4})
  (step2 [1 7]
         #{2 7 9})
  (step2 [1 3 4 5 7 8 10 11 13]
         #{1 3 5 7 9 13})


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

  (runner2 [".......S......."
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
            "..............."]) ;; 40

  (with-open [r (io/reader (io/resource "aoc-2025/day7.txt"))]
    (runner (line-seq r))) ;; {:split-count 1628, :tachyons #{0 70 62 74 110 86 20 72 58 60 101 102 135 88 46 4 106 54 92 137 104 48 50 116 99 113 32 40 129 91 117 108 56 22 90 36 118 100 131 122 44 6 28 134 64 51 25 34 12 2 66 23 127 82 76 97 68 112 138 14 45 78 132 26 140 16 120 38 126 124 30 96 10 18 52 114 67 42 80 94 8 84}}

  (with-open [r (io/reader (io/resource "aoc-2025/day7.txt"))]
    (runner2 (line-seq r)))

  ;; end comment
  )
