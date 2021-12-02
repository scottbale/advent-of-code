(ns aoc-2021.day2
  "Submarine instructions"
  (:require
   [aoc-2020.day2 :refer [nm]]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [debugger :refer [dbg]]
   [the.parsatron :as p]))

(defn fresh-state
  "Returns a pair of numbers: [horizontal position, depth]"
  []
  [0,0])

(defn forward
  [arg [x d]]
  [(+ x arg) d])

(defn down
  [arg [x d]]
  [x (+ d arg)])

(defn up
  [arg [x d]]
  [x (- d arg)])

(def instrs {:forward forward
             :down down
             :up up})

(p/defparser parse-inst []
  (p/let->> [inst-str (p/choice
                       (p/string "forward")
                       (p/string "up")
                       (p/string "down"))
             _ (p/char \ )
             digit (p/digit)]
            (let [inst (keyword inst-str)
                  arg (-> digit str edn/read-string)]
              (p/always [inst arg]))))

(defn runner
  "Parse the input into a program. Initialize a fresh state. Reduce using state and program to a final
  state. The program is a sequence of instructions, each of which 'modifies' the state. Multiply the
  horizontal position by the depth to get the final answer."
  [input]
  (let [parser (parse-inst)
        program (map (partial p/run parser) input)
        state (fresh-state)
        reduce-fn (fn [s [inst arg]]
                    ((instrs inst) arg s))
        [x d] (reduce reduce-fn state program)]
    (* x d)))

(comment

  (p/run (parse-inst) "down 5")

  (->> (fresh-state)
       (forward 3)
       (down 3)
       (up 2))

  ;; Run with test input from the puzzle description
  (runner ["forward 5"
           "down 5"
           "forward 8"
           "up 3"
           "down 8"
           "forward 2"])

  ;; Run with the real input, get the final answer
  (with-open [r (io/reader (io/resource "aoc-2021/day2.txt"))]
    (runner (line-seq r))) ;; 2120749


  )
