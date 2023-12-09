(ns lib.grid
  "A small library to treat a sequence of data as a 2D grid.")

(def width (comp count first))
(def height count)

(defn alist
  "Sometimes it's convenient to build an association list mapping each input item to its index."
  [input]
  (map vector input (range)))

(defn adjacents
  "Given `w` width and `h` height of a `w` x `h` grid, and `n` one of the spots on the
  grid (zero-based), return the seq of two to four points adjacent to `n`"
  [w h n]
  (let [b (* w (long (/ n w))) ;; begin row
        e (+ b w -1)] ;; end row
    (concat
     (for [r [(dec n) (inc n)] :when (and (>= r b) (<= r e))] r)
     (for [c [(- n w) (+ n w)] :when (and (>= c 0) (< c (* w h)))] c))))

(defn neighbors
  "Given `w` width and `h` height of a `w` x `h` grid, and `n` one of the spots on the
  grid (zero-based), return the seq of three to eight points adjacent to `n` (including diagonal
  neighbors)"
  [w h n]
  (let [max-n (* w h)]
    (apply concat
           ;; rows (outer) and columns (inner) with nested list comprehensions
           (for [r [(- n w) n (+ n w)]
                 :when (>= r 0)]
             (let [b (* w (long (/ r w))) ;; row beginning
                   e (+ b w -1)]          ;; row ending
               (for [c [(dec r) r (inc r)]
                     :when (and
                            (not (== c n))
                            (>= c 0)
                            (< c max-n)
                            (>= c b)
                            (<= c e))]
                 c))))))

(comment

  (alist "1234557w89erw")

  (->>
   [:0 1 :2 :3 4 :5 :6]
   alist
   (filter (comp number? first))
   (map second))

  (let [digits #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9}]
    (->>
     ".348..2839@...39823..."
     (partition-by (comp nil? digits))
     (filter (comp digits first))
     (map #(apply str %) )
     )) ;; ("348" "2839" "39823")

  ;; get sequences of indexes of number strings in the input
  (let [digits #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9}]
    (->>
     ".348..2839@...39823..."
     alist
     (partition-by (comp nil? digits first))
     (filter (comp digits first first))
;;     (map #(map second %))
     )) ;; ((1 2 3) (6 7 8 9) (14 15 16 17 18))




  (alist
   ["5483143223"
    "2745854711"
    "5264556173"
    "6141336146"
    "6357385478"
    "4167524645"
    "2176841721"
    "6882881134"
    "4846848554"
    "5283751526"])

  (adjacents 4 5 5)
  (adjacents 4 5 0)
  (adjacents 4 5 19)
  (adjacents 4 5 11)
  (adjacents 4 5 12)
  (adjacents 10 5 5)
  (adjacents 100 100 9998)
  (adjacents 100 100 9900)
  (adjacents 10 10 90)
  (adjacents 3 3 6)
  (adjacents 2 2 2)

  (neighbors 4 5 5)
  (neighbors 4 5 0)

  ;; dbg
  (neighbors 10 10 9)
  (neighbors 10 10 89)

  ;; 0 1 2
  ;; 3 4 5
  ;; 6 7 8

  (neighbors 3 3 4)
  (neighbors 3 3 7)
  (neighbors 3 3 8)

  ;; end comment
  )
