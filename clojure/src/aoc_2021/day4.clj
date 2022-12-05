(ns aoc-2021.day4
  "Giant squid bingo"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as s]
   [debugger :refer [dbg]]))

(def win-indices
  "Ten seqs of vectors of five numbers representing the ten rows and columns that can result in a
  bingo win. Each number is an index into the board, which is a simple seq of 25 numbers."
  [[0 1 2 3 4]
   [5 6 7 8 9]
   [10 11 12 13 14]
   [15 16 17 18 19]
   [20 21 22 23 24]
   [0 5 10 15 20]
   [1 6 11 16 21]
   [2 7 12 17 22]
   [3 8 13 18 23]
   [4 9 14 19 24]])

(defn fresh-board
  "Return a board initialized with 25 numbers. Internally, stored as seq of 25 [number boolean] pairs
  with the boolean indicating if that number has been selected or not. Also stored is the :drawn
  sequence of numbers (all drawn, whether matching on the board or not)"
  [numbers]
  {:numbers (map vector numbers (repeat false))
   :drawn []})

(defn play-drawn-number
  "Given current board and a drawn number, play the number on the board if possible and return a new
  board representing updated state, else return the unaltered board. If board is already a winner..."
  [{:keys [numbers drawn :as board]} number]
  (letfn [(splicer [already-checked [[nm seen? :as entry] & more]]
            (cond 
              (nil? nm) nil
              (== nm number) (concat (conj already-checked [nm true]) more) 
              :keep-looking (recur (conj already-checked entry) more)
              ))]
    (if-let [found (splicer [] numbers)]
      {:numbers found :drawn (conj drawn number)}
      {:numbers numbers :drawn (conj drawn number)})))

(defn winner?
  "A board is a winner if any of the ten rows or cols (see win-indices) has all five numbers selected"
  [{:keys [numbers]}]
  (letfn [(winner-along-row-or-col? [indices]
            (every? (fn [i] (->> i (nth numbers) last true?)) indices))]
    (true? (some winner-along-row-or-col? win-indices))))

(defn drawn-count
  "Return the count of drawn numbers for the board"
  [{:keys [drawn] :as board}] (count drawn))

(defn winning-score
  "Score of the winning board: sum of all unmarked numbers * winning number"
  [{:keys [numbers drawn] :as winning-board}]
  (let [final-drawn (last drawn)
        unmarked-numbers (filter (fn [[_ drawn?]] (false? drawn?)) numbers)
        unmarked-count (reduce + (map first unmarked-numbers))]
    (* final-drawn unmarked-count)))

(defn play-to-completion
  "Play board until win or input is exhausted, return final board"
  [input board]
  (letfn [(play-to-win [board]
            (if (winner? board)
              (reduced board)
              board))]
    (reduce (comp play-to-win play-drawn-number) board input)))

(defn determine-winner
  "With drawn numbers, determine winner among boards."
  [input boards]
  ;; Reduce using seq of boards, with val starting as all drawn numbers but at each step changing to
  ;; (shortened) seq of drawn numbers of previous board
  ;; Write function to be used in a reduction: memo, item
  ;; Have to remember the played boards to determine winner. 
  (letfn [(reducer [{:keys [played-boards drawn]} board]
            (let [played-board (play-to-completion drawn board)]
              {:played-boards (conj played-boards played-board)
               :drawn (:drawn played-board)}))]
    (let [init-state {:played-boards []
                      :drawn input}
          final-state (reduce reducer init-state boards)]
      (->> final-state
           :played-boards
           reverse
           (drop-while (comp not winner?)) ;; TODO computing `winner?` twice for some boards - memoize?
           first))))

(defn runner-pt1
  "A game is the drawn numbers (up through winning number?) and one or more boards (identify winning board?).
  A board is a seq of 25 numbers and some way to mark a spot as 'marked'? Have to sum unmarked numbers."
  [input]
  ;; play board for sequence of input
  ;; winning board pred
  ;; count board unmarked spots
  ;; parse input -> initial state: drawn number seq, N boards
  ;; For each board
  ;;   Play a board to win (or exhaust input)
  ;;   Repeat for each subsequent board, taking the (shortened) input from previous board win
  ;; Repeat until winner in shortest input is found (5, or boards are exhausted - what about ties?)
  (let [drawn (edn/read-string (str \[ (first input) \]))
        boards-raw (drop 2 input)
        raw-partitions (partition-all 5 6 boards-raw)
        boards-strs (map (fn [board-strs] (str \[ (s/join \, board-strs) \])) raw-partitions)
        boards (map (comp fresh-board edn/read-string) boards-strs)
        winner (determine-winner drawn boards)]
    (winning-score winner)))

(defn play-all-to-completion
  "Like `determine-winner`, but play all boards to completion with the original input, and return them all"
  ;; Pt 1 could be reimplemented to work like pt 2, except with a `first-winner` function. I thought
  ;; I was being so clever in `determine-winner` by optimizing the search by only using the drawn
  ;; number seq from the previous board, but that didn't play all boards to completion, it was only
  ;; optimized for the pt 1 case of finding the first place winner. And did it really save much time?
  [input boards]
  (letfn [(reducer [played-boards board]
            (conj played-boards (play-to-completion input board)))]
    (reduce reducer [] boards)))

(def last-winner
  "Of the winning boards, find the one that would have won last"
  ;; yep, showing off with `comp` and `partial`
  (comp last (partial sort-by drawn-count) (partial filter winner?)))

(defn runner
  "For part 2 determine last board to win"
  [input]
  (let [drawn (edn/read-string (str \[ (first input) \]))
        boards-raw (drop 2 input)
        raw-partitions (partition-all 5 6 boards-raw)
        boards-strs (map (fn [board-strs] (str \[ (s/join \, board-strs) \])) raw-partitions)
        boards (map (comp fresh-board edn/read-string) boards-strs)
        played-boards (play-all-to-completion drawn boards)]
    (winning-score (last-winner played-boards))))

(comment

  ;; pt 2

  (with-open [r (io/reader (io/resource "aoc-2021/day4.txt"))]
    (runner (line-seq r))) ;; 4624

  (runner ["7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
           ""
           "22 13 17 11  0"
           "8  2 23  4 24"
           "21  9 14 16  7"
           "6 10  3 18  5"
           "1 12 20 15 19"
           ""
           "3 15  0  2 22"
           "9 18 13 17  5"
           "19  8  7 25 23"
           "20 11 10 24  4"
           "14 21 16 12  6"
           ""
           "14 21 17 24  4"
           "10 16 15  9 19"
           "18  8 23 26 20"
           "22 11 13  6  5"
           "2  0 12  3  7"])

  ;; pt 1

  (with-open [r (io/reader (io/resource "aoc-2021/day4.txt"))]
    (runner-pt1 (line-seq r))) ;; 65325

  (runner-pt1 ["7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
               ""
               "22 13 17 11  0"
               "8  2 23  4 24"
               "21  9 14 16  7"
               "6 10  3 18  5"
               "1 12 20 15 19"
               ""
               "3 15  0  2 22"
               "9 18 13 17  5"
               "19  8  7 25 23"
               "20 11 10 24  4"
               "14 21 16 12  6"
               ""
               "14 21 17 24  4"
               "10 16 15  9 19"
               "18  8 23 26 20"
               "22 11 13  6  5"
               "2  0 12  3  7"])

  (let [input [7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1]
        board (fresh-board [14 21 17 24 4 10 16 15  9 19 18  8 23 26 20 22 11 13  6  5 2  0 12  3  7])]
    board)

  ;; winning board and input
  (let [input [7 4 9 5 11 17 23 2 0 14 21 24]
        board (fresh-board [14 21 17 24 4 10 16 15  9 19 18  8 23 26 20 22 11 13  6  5 2  0 12  3  7])
        final-board (play-to-completion input board)]
    
    {:winner? (winner? final-board)
     :win-count (drawn-count final-board)
     :score (winning-score final-board)
     :final-board final-board
     })

  ;; non-winning board and input
  (let [input [7 4 9 5 11 17 23 2 0 14 21 24]
        board (fresh-board [14 21 17 99 4 10 16 15  9 19 18  8 23 26 20 22 11 13  6  5 2  0 12  3  7])
        final-board (play-to-completion input board)]
    
    {:winner? (winner? final-board)
     :drawn-count (drawn-count final-board)
     :final-board final-board
     })

  ;; play winning board only to completion
  (let [input [7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1]
        board (fresh-board [14 21 17 24 4 10 16 15  9 19 18  8 23 26 20 22 11 13  6  5 2  0 12  3  7])
        final-board (play-to-completion input board)]
    
    {:winner? (winner? final-board)
     :win-count (drawn-count final-board)
     :score (winning-score final-board)
     :winning-numbers (:drawn final-board)
     ;; :final-board final-board
     })

  (reduced :foo)

  (let [board (fresh-board [14 21 17 24 4 10 16 15  9 19 18  8 23 26 20 22 11 13  6  5 2  0 12  3  7])
        final-board (play-drawn-number board 21)]
    (count (:numbers final-board))
    final-board)


  (let [input [7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1]
        boards [(fresh-board [22 13 17 11  0
                              8  2 23  4 24
                              21  9 14 16  7
                              6 10  3 18  5
                              1 12 20 15 19])
                ;; winning
                (fresh-board [14 21 17 24 4 10 16 15  9 19 18  8 23 26 20 22 11 13  6  5 2  0 12  3  7])
                (fresh-board [ 3 15  0  2 22
                              9 18 13 17  5
                              19  8  7 25 23
                              20 11 10 24  4
                              14 21 16 12  6])]]
    (determine-winner input boards))

  (drop-while neg? [-12 -13 3 18 -1 -2 3 4 5])

  (->> (s/split "1,2,3,4,5" #",")
       (map edn/read-string))

  (edn/read-string (str \[ "1,2,3,4,5" \]))

  (let [boards-raw ["22 13 17 11  0"
                    "8  2 23  4 24"
                    "21  9 14 16  7"
                    "6 10  3 18  5"
                    "1 12 20 15 19"
                    ""
                    "3 15  0  2 22"
                    "9 18 13 17  5"
                    "19  8  7 25 23"
                    "20 11 10 24  4"
                    "14 21 16 12  6"
                    ""
                    "14 21 17 24  4"
                    "10 16 15  9 19"
                    "18  8 23 26 20"
                    "22 11 13  6  5"
                    "2  0 12  3  7"]
        raw-partitions (partition-all 5 6 boards-raw)
        boards-strs (map (fn [board-strs] (str \[ (s/join \, board-strs) \])) raw-partitions)]
    (map (comp fresh-board edn/read-string) boards-strs))

  ;; play all boards to completion
  (let [input [7 4 9 5 11 17 23 2 0 14 21 24 10 16 13 6 15 25 12 22 18 20 8 19 3 26 1]
        boards [(fresh-board [22 13 17 11  0
                              8  2 23  4 24
                              21  9 14 16  7
                              6 10  3 18  5
                              1 12 20 15 19])
                ;; winning
                (fresh-board [14 21 17 24 4 10 16 15  9 19 18  8 23 26 20 22 11 13  6  5 2  0 12  3  7])
                (fresh-board [ 3 15  0  2 22
                              9 18 13 17  5
                              19  8  7 25 23
                              20 11 10 24  4
                              14 21 16 12  6])]]
    (play-all-to-completion input boards))

  )
