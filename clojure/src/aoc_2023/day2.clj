(ns aoc-2023.day2
  "Cube Conundrum. Bags of different color cubes. What games are possible?"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as s]))

(defn parse-cubes
  "parse a single cube qty/color pair string, return as a map of keyword to long"
  [cubes-str]
  (let [[n color](-> cubes-str s/trim (s/split #" "))]
    {(keyword color) (edn/read-string n)}))

(defn parse-cubes-list
  "return a map"
  [cubes-list-str]
  (apply merge (map parse-cubes (-> cubes-list-str s/trim (s/split #",")))))

(defn parse-game-list
  "Parse a game list of cubes lists; return a list maps"
  [game-list-str]
  (map parse-cubes-list (-> game-list-str s/trim (s/split #";"))))

(defn parse-game
  [game-str]
  (let [[id-str game-list-str] (s/split game-str #":")
        id (-> id-str (s/split #" ") last edn/read-string)
        game-list (parse-game-list game-list-str)]
    {:id id :game-list game-list}))

(defn cube-pairing-in-the-bag?
  "Predicate, returns true if the cube pairing (a map entry) is possible given the contents of the bag."
  [bag [k v]]
  (<= v (get bag k 0)))

(defn possible-game?
  "Given the bag (a map of color keyword to qty long), is the game possible? The game is a collection
  of maps of similar structure. The game is possible unless the quantity of a single color exceeds
  the bag."
  [bag {:keys [game-list]}]
  (letfn [(in-the-bag? [cubes-pairings]
            (every? (partial cube-pairing-in-the-bag? bag) cubes-pairings))]
    (every? in-the-bag? game-list)))

(defn runner1
  "runner part 1"
  [input]
  (let [bag {:blue 14 :red 12 :green 13}]
    (->> input
         (map parse-game)
         (filter (partial possible-game? bag))
         (map :id)
         (reduce +))))

(defn cubes-power [{:keys [red green blue] :or {red 0 green 0 blue 0}}]
  (* red green blue))

(defn runner2
  "runner part 2"
  [input]
  (->> input
       (map (comp cubes-power #(apply merge-with max %) :game-list parse-game))
       (reduce +)))


(comment

  (runner1 ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
            "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
            "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
            "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
            "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"]) ;; 8

  (runner2 ["Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
            "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
            "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
            "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
            "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"]) ;; 2286

  (with-open [r (io/reader (io/resource "aoc-2023/day2.txt"))]
    (runner1 (line-seq r))) ;; 2369

  (with-open [r (io/reader (io/resource "aoc-2023/day2.txt"))]
    (runner2 (line-seq r))) ;; 66363

  (merge-with max {:blue 3 :green 2} {:blue 2 :green 3 :red 5})

  (cubes-power {:red 2 :green 3})
  (cubes-power {:red 2 :green 3 :blue 5})

  (possible-game?
   {:blue 3 :red 4}
   {:id 1, :game-list '({:blue 3, :red 4})}) ;; true
  (possible-game?
   {:blue 3 :red 4}
   {:id 1, :game-list '({:blue 3, :red 4} {:red 1, :green 2, :blue 6} {:green 2})}) ;; false
  (possible-game?
   {:blue 99 :red 99 :green 99}
   {:id 1, :game-list '({:blue 3, :red 4} {:red 1, :green 2, :blue 6} {:green 2})}) ;; true
  (possible-game?
   {}
   {:id 1, :game-list '({:blue 3, :red 4} {:red 1, :green 2, :blue 6} {:green 2})}) ;; false

  (cube-pairing-in-the-bag? {:red 4} [:red 3])  ;; true
  (cube-pairing-in-the-bag? {:red 2} [:red 3])  ;; false
  (cube-pairing-in-the-bag? {:red 3} [:red 3])  ;; true
  (cube-pairing-in-the-bag? {:blue 2} [:red 3]) ;; false

  (parse-cubes " 1 red")
  (parse-cubes-list " 3 green, 15 blue, 14 red")
  (parse-game-list " 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red")
  (parse-game "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

;; end comment
  )
