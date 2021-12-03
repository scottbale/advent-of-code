(ns aoc-2020.day11
  "Like Conway's GOL but with airplane seats being occupied or not."
  (:require
   [clojure.java.io :as io]
   [debugger :refer [dbg]]))

(defn occupied? [seat] (= seat \#))

(defn unoccupied? [seat] (= seat \L))

(defn chair? [seat]
  "Returns true unless the seat is a floor spot"
  (not (= seat \.)))

(defn count-occupied [seats]
  (->> seats
       (filter occupied?)
       count))

(defn seat-at [seating [n m :as coords]]
  (nth (nth seating n) m))

(defn flat-seat-at [width flat-seating [n m :as coords]]
  (nth flat-seating (+ m (* width n))))

(defn boundary-indices
  "Generate a sequence of up to 3 numbers"
  [n i]
  (range (max 0 (dec i)) (inc (min n (inc i)))))

(defn adjacent-coordinates-seq
  "Return a sequence of list of pairs of [row, col] coordinates (zero-based) representing adjacent seats of
  each (row,col) coordinate of the input params"
  [nm-rows nm-cols]
  (let [row-nms (range nm-rows)
        col-nms (range nm-cols)
        index-and-adjacents (fn [n] (fn [i] [i (boundary-indices n i)]))]
    (for [[row row-indices] (map (index-and-adjacents (dec nm-rows)) row-nms)
          [col col-indices] (map (index-and-adjacents (dec nm-cols)) col-nms)]
      (for [row-coord row-indices
            col-coord col-indices
            :when (not (and (= row-coord row) (= col-coord col)))]
        [row-coord col-coord]))))

(defn seat-next-gen [seating seat adjacent-seat-coords]
  (if (chair? seat)
    (let [adjacent-seats (map (partial seat-at seating) adjacent-seat-coords)
          occupied-count (count-occupied adjacent-seats)]
      (cond
        (and (unoccupied? seat) (== 0 occupied-count)) \#
        (and (occupied? seat) (<= 4 occupied-count)) \L
        :else seat))
    seat))

(defn next-gen [adjacent-coordinates-seq width seating]
  (let [seating-flattened (apply concat seating)
        next-seat-fn (fn [seat adjacent-seat-coords]
                       (seat-next-gen seating seat adjacent-seat-coords))
        next-seating (map next-seat-fn seating-flattened adjacent-coordinates-seq)]
    (->> next-seating
         (partition width)
         (map (partial apply str)))))

(defn stabilize
  "Keep next-gen'ing until prev and current gen are the same"
  [coord-seq width seating]
  (letfn [(step [seating safety-valve]
            (println ">>>>step" safety-valve)
            (let [next-seating (next-gen coord-seq width seating)]
              (if (or (= seating next-seating) (== 0 safety-valve))
                next-seating
                (recur next-seating (dec safety-valve)))))]
    (step seating 100)))

(defn runner [input]
  (let [n (count input)
        width (count (first input))
        coord-seq (adjacent-coordinates-seq n width)
        final-gen (stabilize coord-seq width input)]
    (count-occupied (apply concat final-gen))))

(comment


  ((fn [x y]
        (let [n 40
              w 8
              ]
          (+ (* w y) x))) 2 3)

  ((fn [seat]
        (let [n 40
              w 8
              x (mod seat w)
              y (/ (- seat x) w)]
          [x y])) 26)

  (runner ["L.LL.LL.LL"
           "LLLLLLL.LL"
           "L.L.L..L.."
           "LLLL.LL.LL"
           "L.LL.LL.LL"
           "L.LLLLL.LL"
           "..L.L....."
           "LLLLLLLLLL"
           "L.LLLLLL.L"
           "L.LLLLL.LL"])

  (time (with-open [r (io/reader (io/resource "aoc-2020/day11.txt"))]
     (runner (line-seq r)))) ;;2483   ;; 3551 on test run ;; 8 seconds to run

  (runner ["L#"
           "LL"])

  (runner ["LLL"
           "L.L"
           "LLL"])

  (flat-seat-at 10 (apply str ["L.LL.LL.LL"
                               "LLLLLLL.LL"
                               "L.L.L..L.."
                               "LLLL.LL.LL"
                               "L.LL.LL.LL"
                               "L.LLLLL.LL"
                               "..L.L....."
                               "LLLLLLLLLL"
                               "L.LLLLLL.L"
                               "L.LLLLL.LL"]) [9 7])
  
  (count "L.LL.LL.LL")
  (boundary-indices 9 9)

  ;; 2x2 test case
  (adjacent-coordinates-seq 2 2)

  ;; 3x3 test case
  (adjacent-coordinates-seq 3 3)

  ;; each item should be a list of pairs of coordinates (representing adjacent seats) to check
  ;; 2x2 expected:
  [
   [ [0 1] [1 0] [1 1]]                 ;for seat (0,0)
   [ [0 0] [1 0] [1 1]]                 ;for seat (0,1)
   [ [0 0] [0 1] [1 1]]                 ;for seat (1,0)
   [ [0 0] [0 1] [1 0]]                 ;for seat (1,1)
   ]

  ;; 3x3 expected: (should be from three to eight pairs per seat, depending on boundaries)
  [
   ;; row 0 (pair count is three or five)
   [ [0 1] [1 0] [1 1]]                               ;for seat (0,0)
   [ [0 0] [0 2] [1 0] [1 1] [1 2]]                   ;for seat (0,1)
   [ [0 1] [1 1] [1 2]]                               ;for seat (0,2)
   ;; row 1 (pair count is five or eight)
   [ [0 0] [0 1] [1 1] [2 0] [2 1]]                   ;for seat (1,0)
   [ [0 0] [0 1] [0 2] [1 0] [1 2] [2 0] [2 1] [2 2]] ;for seat (1,1)
   ;; ...
   ;; row 2 (pair count is three or five)
   ;; ...
   ]

  ;; actual
  (([0 1] [1 0] [1 1]) 
   ([0 0] [0 2] [1 0] [1 1] [1 2]) 
   ([0 1] [1 1] [1 2]) 
   ([0 0] [0 1] [1 1] [2 0] [2 1]) 
   ([0 0] [0 1] [0 2] [1 0] [1 2] [2 0] [2 1] [2 2]) 
   ([0 1] [0 2] [1 1] [2 1] [2 2]) 
   ([1 0] [1 1] [2 1]) 
   ([1 0] [1 1] [1 2] [2 0] [2 2]) 
   ([1 1] [1 2] [2 1]))

  
  )
