(ns aoc-2023.day4
  "Scratchcards"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as s]))

(defn parse-card
  "Parse a line of input into a card"
  [input]
  (let [spaces #"[ \t]+"
        [winnings haves] (s/split input #"\|")
        [card winnings] (s/split winnings #":")
        winning-number-strs (s/split (s/trim winnings) spaces)
        winning-numbers (into #{} (map edn/read-string winning-number-strs))
        have-numbers (into #{} (map edn/read-string (s/split (s/trim haves) spaces)))]
    {:card card
     :winners winning-numbers
     :haves have-numbers}))

(defn score-card
  "Given a card, assoc its :score."
  [{:keys [winners haves] :as card}]
  (letfn [(scorer [sum have]
            (if (winners have)
              (if (= 0 sum) 1 (* 2 sum))
              sum))]
    (assoc card :score (reduce scorer 0 haves))))

(defn card-matches
  "Given a card, assoc its :matches."
  [{:keys [winners haves] :as card}]
  (letfn [(matches [matches have]
            (if (winners have)
              (inc matches)
              matches))]
    (assoc card :matches (reduce matches 0 haves))))

(defn runner
  "Sum the score of all the cards."
  [input]
  (->> input (map (comp :score score-card parse-card)) (reduce +)))

(defn cards-map
  [cards]
  (letfn [(reducer [m {:keys [card]}]
            (assoc m card 1))]
    (reduce reducer {} cards)))

(defn runner-pt2
  "Rather than points, the # of matches N of each cards duplicates the next N cards. Calculate the
  total # of cards."
  [input]
  (let [cards (->> input (map (comp card-matches parse-card)))
        cards-m (cards-map cards)
        cards-m' (loop [[current-card & rest-of-cards] cards m cards-m]
                   (if (seq rest-of-cards)
                     (let [{:keys [card matches]} current-card
                           factor (m card)
                           updated-m (if (not (nil? matches))
                                       (reduce (fn [m' {:keys [card]}]
                                                 (update m' card #(+ % factor))) m (take matches rest-of-cards))
                                       m)]
                       (recur rest-of-cards updated-m))
                     m))]
    (->> cards-m' vals (reduce +))))



(comment

  (s/split "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83" #"\|")

  (s/split "59 84 76 51 58  5 54 83" #"[ \t]+")

  (parse-card "Card 4: 41 9  73 84 69 | 59 84 76 51 58  5 54 83")
  (score-card {:winners #{69 41 92 73 84}, :haves #{59 58 54 51 76 5 83 84}})
  (-> "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53" parse-card score-card)
  (-> "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53" parse-card card-matches)
  (-> "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53" parse-card (vector) cards-map)

  (runner ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
           "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
           "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
           "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
           "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
           "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"]) ;; 13

  (with-open [r (io/reader (io/resource "aoc-2023/day4.txt"))]
    (runner (line-seq r))) ;; 24160

  (runner-pt2 ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
               "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19"
               "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1"
               "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
               "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
               "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"]) ;; 30

  (with-open [r (io/reader (io/resource "aoc-2023/day4.txt"))]
    (runner-pt2 (line-seq r))) ;; 5659035

  ;; end comment
  )
