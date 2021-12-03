(ns aoc-2020.day11-pt2
  "Like Conway's GOL but with airplane seats being occupied or not."
  (:require
   [clojure.java.io :as io]
   [debugger :refer [dbg]]))

(defn occupied? [seat] (= seat \#))

(defn unoccupied? [seat] (= seat \L))

(defn floor? [seat]
  "Returns true if the seat is a floor spot"
  (= seat \.))

(defn count-occupied [seats]
  (->> seats
       (filter occupied?)
       count))

(defn seat-at [seating [x y]]
  (nth (nth seating x) y))

(defn seat-coords
  "For a seating area width by n, return a sequence of seat coordinate pairs, x (row) and y (col) representing every seat."
  [width n]
  (let [xs (range n)
        ys (range width)]
    (for [x xs y ys] [x y])))

(defn seat-seek-space
  "For a seating matrix width by n, and a seat identified by row and col, return the neighboring seats that must be checked. In this case, that is the line-of-sight seats eminating out from the indicated seat in each of the eight cardinal directions."
  [width n [row col]]
  {
   :N (map vector (range (dec row) -1 -1) (repeat col))
   :NE (map vector (range (dec row) -1 -1) (range (inc col) width))
   :E (map vector (repeat row) (range (inc col) width) )
   :SE (map vector (range (inc row) n) (range (inc col) width))
   :S (map vector (range (inc row) n) (repeat col))
   :SW (map vector (range (inc row) n) (range (dec col) -1 -1))
   :W (map vector (repeat row) (range (dec col) -1 -1) )
   :NW (map vector (range (dec row) -1 -1) (range (dec col) -1 -1))
   })

(defn occupied-count-of-first-seat-visible-along-axis
  "Return 0 or 1, depending on whether the first seat (if any) that can be seen along the seats path is occupied or not."
  [seating seats]
  (letfn [(step [[seat-xy & seat-xys]]
            (if seat-xy
              (let [seat (seat-at seating seat-xy)]
                (if (floor? seat) 
                  (recur seat-xys)
                  (if (occupied? seat) 1 0)))
              0))]
    (step seats)))

(defn occupied-sum-visible-from-seat
  "The count of occupied seats visible in all directions encapsulated by the seek space"
  [seek-space seating]
  (->> (map 
        (partial occupied-count-of-first-seat-visible-along-axis seating) 
        (vals seek-space))
       (reduce +)))

(defn seat-next-gen
  "Given the seat state and the count of nearby occupied seats return the next seat state"
  [seat occupied-count]
  (cond
    (floor? seat) seat
    (occupied? seat) (if (> 5 occupied-count) seat \L)
    (unoccupied? seat) (if (== 0 occupied-count) \# seat)
    :default seat))

(defn next-gen
  "Given the precomputed stuff (first two params) and the current seating gen, return the next gen of seat values"
  [width seat-xys seek-spaces seating]
  (letfn [(nu-seat [seat-xy seat-seek-space]
            (seat-next-gen 
             (seat-at seating seat-xy) 
             (occupied-sum-visible-from-seat seat-seek-space seating)))]
    (partition width (map nu-seat seat-xys seek-spaces))))

(defn dbg-print [seating]
  (println (clojure.string/join "\n" (map (partial apply str) seating)))
  seating)

(defn runner [input]
  (let [n (count input)
        width (count (first input))
        seat-xys (seat-coords width n)
        seek-spaces (map (partial seat-seek-space width n) seat-xys)]
    (letfn [(stabilize [current-gen safety-valve]
              (println ">>>>>>safety-valve" safety-valve)
              (let [ng (next-gen width seat-xys seek-spaces current-gen)]
                (if (or (= current-gen ng) (== 0 safety-valve))
                  ng
                  (recur ng (dec safety-valve)))))]
      (count-occupied (apply concat (stabilize input 100))))))



(comment

  (dbg-print [[\. \L \.] [\# \L \.] [\L \# \#]])

  (runner ["L.L"
           "LLL"
           "L.L"])

  (occupied-count-of-first-seat-visible-along-axis 
   [".L#"] [[0 0] [0 1] [0 2]])



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
          (runner (line-seq r)))) ;;2285 90 iterations 30 sec

  (seat-seek-space 4 4 [0 0])
  (seat-coords 2 2)

  (seat-next-gen \. 3)
  (seat-next-gen \. 0)
  (seat-next-gen \. 6)
  (seat-next-gen \L 3)
  (seat-next-gen \L 0)
  (seat-next-gen \L 6)
  (seat-next-gen \# 3)
  (seat-next-gen \# 0)
  (seat-next-gen \# 6)

  

  )
