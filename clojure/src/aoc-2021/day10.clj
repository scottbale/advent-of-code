(ns aoc-2021.day10
  "Syntax scoring"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [debugger :refer [dbg]]))

(def scores {\) 3 \] 57 \} 1197 \> 25137})

(def delims {\( \)
             \[ \]
             \{ \}
             \< \>})

(def closing?
  "The char is a closing delimiter if it is in the `scores` keyset"
  (set (keys scores)))

(defn reduce-line
  "For the next delimiter in the input, return the updated open delimeter stack and score"
  [[score [open-delim & more :as delim-stack] :as state] next-delim]
  (if (closing? next-delim)
    (if (= next-delim (delims open-delim))
      [score more]
      (reduced [(+ score (scores next-delim)) delim-stack]))
    [score (cons next-delim delim-stack)]))

(defn score-line
  "Process a line of input, return a score of zero or more"
  [line]
  (first (reduce reduce-line [0 []] line)))

(defn runner-pt1
  ""
  [input]
  (reduce + (map score-line input)))

(comment

  ;; pt 1

  (runner-pt1 ["[({(<(())[]>[[{[]{<()<>>" 
               "[(()[<>])]({[<{<<[]>>(" 
               "{([(<{}[<>[]}>{[]{[(<()>" 
               "(((({<>}<{<{<>}{[]{[]{}" 
               "[[<[([]))<([[{}[[()]]]" 
               "[{[{({}]{}}([{[{{{}}([]" 
               "{<[[]]>}<{[{[{[]{()[[[]" 
               "[<(<(<(<{}))><([]([]()" 
               "<{([([[(<>()){}]>(<<{{" 
               "<{([{{}}[<[[[<>{}]]]>[]]"]) ;; 26397

  (with-open [r (io/reader (io/resource "aoc-2021/day10.txt"))]
    (runner-pt1 (line-seq r))) ;; 319233

  (closing? \?)
  (reduce-line [0 [\[ \{]] \<)
  (reduce-line [0 [\[ \{]] \])
  (reduce-line [0 [\[ \{]] \>)
  (score-line "[({(<(())[]>[[{[]{<()<>>")


  )
