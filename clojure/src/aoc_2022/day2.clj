(ns aoc-2022.day2
  "rock paper scissors"
  (:require
   [clojure.java.io :as io]))

;; pt 1
(def canonical-moves {\A :rock \B :paper \C :scissors \X :rock \Y :paper \Z :scissors})
(def move-shape->score {:rock 1 :paper 2 :scissors 3})
(def win-lose-draw-scores {:rock {:scissors 6 :paper 0 :rock 3}
                           :paper {:rock 6 :scissors 0 :paper 3}
                           :scissors {:paper 6 :rock 0 :scissors 3}})
;; pt 2
(def canonical-outcomes {\X :lose \Y :draw \Z :win})
(def move+outcome->move {:rock {:win :paper :lose :scissors :draw :rock}
                         :paper {:win :scissors :lose :rock :draw :paper}
                         :scissors {:win :rock :lose :paper :draw :scissors}})

(defn parse-move
  "input is a string pair of moves e.g. 'B X'. Output is a vector pair of keywords."
  [[move1 _ move2]]
  [(canonical-moves move1) (canonical-moves move2)])

(defn parse-move-and-outcome
  "input is a string pair of move and outcome e.g. 'B X'. Output is a vector pair of keywords."
  [[move _ outcome]]
  [(canonical-moves move) (canonical-outcomes outcome)])

(defn determine-move
  "For part 2, the second value in the pair is the desired match outcome. Convert that to what your
  move should be to bring about that outcome, and return the pair of [elf move, your move]."
  [[elf-move outcome]]
  [elf-move ((move+outcome->move elf-move) outcome)])

(defn score-move
  "Given a vector pair of keywords (first is elf move, second is my your), return a number
  representing the score. The score is the sum of the win/lose/draw (6/0/3) plus the score of your
  chosen move shape."
  [[elf-move your-move]]
  (+ ((win-lose-draw-scores your-move) elf-move) (move-shape->score your-move)))

(defn runner1
  "Sum the scores of each input pair. Each input pair is an elf move plus your move."
  [input-pairs]
  (reduce + (map (comp score-move parse-move) input-pairs)))

(defn runner
  "Sum the scores of each input pair. Each input pair is an elf move plus what should be the outcome
  of the match."
  [input-pairs]
  (reduce + (map (comp score-move determine-move parse-move-and-outcome) input-pairs)))


(comment

  (runner ["A Y"
           "B X"
           "C Z"]) ;; 12 ;; 15

  (with-open [r (io/reader (io/resource "aoc-2022/day2.txt"))]
    (runner (line-seq r))) ;; 13433 ;; 13484

  (parse-move "A Y")
  (score-move [:rock :paper])
  (score-move [:paper :rock])
  (score-move [:rock :scissors])

  ;; pt 2
  (parse-move-and-outcome "A Y")
  (determine-move [:rock :draw])


  )
