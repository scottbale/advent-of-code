(ns aoc-2022.day2
  "rock paper scissors"
  (:require
   [clojure.java.io :as io]))

(def canonical-moves {\A :rock \B :paper \C :scissors \X :rock \Y :paper \Z :scissors})
(def move-shape->score {:rock 1 :paper 2 :scissors 3})
(def win-lose-draw-scores {:rock {:scissors 6 :paper 0 :rock 3}
                           :paper {:rock 6 :scissors 0 :paper 3}
                           :scissors {:paper 6 :rock 0 :scissors 3}})

(defn parse-move
  "input is a string pair of moves e.g. 'B X'. Output is a vector pair of keywords."
  [[move1 _ move2]]
  [(canonical-moves move1) (canonical-moves move2)])

(defn score-move
  "Given a vector pair of keywords (first is elf move, second is my your), return a number
  representing the score. The score is the sum of the win/lose/draw (6/0/3) plus the score of your
  chosen move shape."
  [[elf-move your-move]]
  (+ ((win-lose-draw-scores your-move) elf-move) (move-shape->score your-move)))

(defn runner
  "runner docstring"
  [input-pairs]
  (reduce + (map (comp score-move parse-move) input-pairs)))


(comment

  (runner ["A Y"
           "B X"
           "C Z"]) ;; 15

  (with-open [r (io/reader (io/resource "aoc-2022/day2.txt"))]
    (runner (line-seq r))) ;; 13484

  (parse-move "A Y")
  (score-move [:rock :paper])
  (score-move [:paper :rock])
  (score-move [:rock :scissors])


  )
