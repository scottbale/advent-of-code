(ns aoc-2022.day5
  "Supply stacks"
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn fresh-stack-state
  "Return a fresh stack state: a vector of `n` empty stacks."
  [n]
  (apply conj [] (repeat n [])))

(defn parse-stack-input-line
  "Parse a single line of input, return a string with a char or space at each stack index. (String
  length may be less than number of stacks.)"
  [input-line]
  (let [parts (partition 4 4 "    " input-line)]
    (->> parts
         (map (fn [[_ crate]] crate))
         (reduce str))))

(defn conj-stack-input
  "Given a stack state and a string of stack values to be pushed onto stacks, return updated stack
  state. `vals` is a single string, with each non-whitespace char value to be pushed onto
  corresponding stack. `stacks` is a vector of vectors."
  [stacks vals]
  (first (reduce
          (fn [[stacks' i] val]
            (if (= \  val)
              [stacks' (inc i)]
              [(assoc stacks' i (conj (nth stacks' i) val)) (inc i)]))
          [stacks 0] vals)))

(defn parse-move
  "Given line of input, return a map representing a move, with keys `:count`, `:from`, `:to`"
  [input]
  (let [[_ _ count _ _ _ from _ _ _ to] (partition-by #(= \  %) input)
        count (->> count (apply str) (Long/parseLong))
        from (->> from (apply str) (Long/parseLong))
        to (->> to (apply str) (Long/parseLong))]
    {:count count :from from :to to}))

(defn move ;; pt1
  "Move `n` items from stack `a` to stack `b`, return pair `[a' b']`"
  [a b n]
  (letfn [(move-one [[a' b']]
            [(pop a')
             (conj b' (peek a'))])]
    (->> [a b]
         (iterate move-one)
         (take (inc n))
         last)))

(defn move2-electric-boogaloo ;; pt2
  "Move `n` items from stack `a` to stack `b`, return pair `[a' b']`. But the CrateMover 9001 moves
  all `n` crates at once!"
  [a b n]
  (let [i (- (count a) n)]
    [(vec (take i a))
     (vec (concat b (drop i a)))]))

(defn apply-move
  "Apply the move to the state, returning a new state. State is a vector of stacks of values. Move
  is a map with keys `:count`, `:from`, `:to`"
  [move-fn stacks {:keys [count from to]}]
  (let [from (dec from) ;; convert 1-based to 0-based
        to (dec to)
        a (nth stacks from)
        b (nth stacks to)
        [a' b'] (move-fn a b count)]
    (-> stacks
        (assoc from a')
        (assoc to b'))))

(defn extract-result
  "Given the vector of stacks, form the final result string by concatenating together each of the top
  crates (values are chars) from each stack."
  [stacks]
  (reduce str (map peek stacks)))

(defn runner1 ;; pt 1
  "Where to start with this one? The first part of the input seq, up to the blank line, is the initial
  state of stacks. After the blank line is the list of moves to perform on the stacks. Use chars as
  values, ultimately assembling a final string from the chars that are the top of each stack after
  all the moves are performed."
  [input]
  (let [[stack-input _ moves-input] (partition-by str/blank? input)
        ;; The last line of stack input is the stack numbering; only care about the last number
        n (-> stack-input last butlast last str Long/parseLong)
        fresh-stacks (fresh-stack-state n)
        ;; reverse the stack input list to build the stacks from the bottom up
        stacks (reduce (fn [stacks' input]
                         (->> input
                              (parse-stack-input-line)
                              (conj-stack-input stacks'))) fresh-stacks (-> stack-input reverse rest))
        moves (map parse-move moves-input)]
    (extract-result (reduce (partial apply-move move) stacks moves))))

(defn runner ;; pt 2
  "Same as runner1 except passes a different function arg to `apply-move`"
  [input]
  (let [[stack-input _ moves-input] (partition-by str/blank? input)
        n (-> stack-input last butlast last str Long/parseLong)
        fresh-stacks (fresh-stack-state n)
        stacks (reduce (fn [stacks' input]
                         (->> input
                              (parse-stack-input-line)
                              (conj-stack-input stacks'))) fresh-stacks (-> stack-input reverse rest))
        moves (map parse-move moves-input)]
    (extract-result (reduce (partial apply-move move2-electric-boogaloo) stacks moves))))


(comment

  (runner1 ["    [D]"
            "[N] [C]"
            "[Z] [M] [P]"
            " 1   2   3 "
            ""
            "move 1 from 2 to 1"
            "move 3 from 1 to 3"
            "move 2 from 2 to 1"
            "move 1 from 1 to 2"]) ;; "CMZ"

  (runner ["    [D]"
           "[N] [C]"
           "[Z] [M] [P]"
           " 1   2   3 "
           ""
           "move 1 from 2 to 1"
           "move 3 from 1 to 3"
           "move 2 from 2 to 1"
           "move 1 from 1 to 2"]) ;; "MCD"

  (with-open [r (io/reader (io/resource "aoc-2022/day5.txt"))]
    (runner (line-seq r))) ;; "TCGLQSLPW" ;; "VWLCWGSDQ"

  (conj-stack-input (fresh-stack-state 3) " D")

  (-> (fresh-stack-state 3)
      (conj-stack-input "ZMP")
      (conj-stack-input "AC")
      (conj-stack-input " D")
      (extract-result))

  (parse-stack-input-line "    [D]")
  (parse-stack-input-line "[Z] [M] [P]")
  (parse-stack-input-line "[L]     [W] [B] [Z]     [F] [S] [V]")

  (parse-move "move 11 from 7 to 2")

  (move [:a :b :c] [:d :e] 3)
  (move2-electric-boogaloo [:a :b :c] [:d :e] 3)

  (apply-move
   move
   [[:a :b :c :d :e]
    [:f :g :h :i :j]
    [:k :l :m :n :o]]
   {:count 3 :from 1 :to 3})

  (extract-result [["A" "B" "C"]
                   ["D" "E" "F"]
                   ["G"]])

  (conj [:a :b :c] :d :e)

  (let [a [:a :b :c :d :e]
        b [:f :g :h :i :j]]
    [(pop a)
     (conj b (peek a))])

  )
