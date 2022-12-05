(ns aoc-2021.day10
  "Syntax scoring"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [debugger :refer [dbg]]))

(def scores {\) 3 \] 57 \} 1197 \> 25137})
(def closing-scores {\) 1 \] 2 \} 3 \> 4})

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
  "Process a line of input, return a pair [score of zero or more, unclosed open-delimiter stack]"
  [line]
  (reduce reduce-line [0 []] line))

(defn complete-line
  "Given the remaining open delim queue of an incomplete lines, return the closing delims that would
  complete the line"
  [open-delims]
  (map delims open-delims))

(defn score-complete-line
  "Score the completing delims"
  [close-delims]
  (reduce (fn [score delim]
            (+ (* score 5) (closing-scores delim))) 0 close-delims))

(defn runner-pt1
  "Sum the score of all corrupted input lines"
  [input]
  (reduce + (map (comp first score-line) input)))

(defn runner-pt2
  "Complete and score the incomplete lines, sort scores, return the middle score"
  [input]
  (let [score-queue-pairs (map score-line input)
        scores (->> score-queue-pairs
                    (remove (comp pos? first))
                    (map (comp score-complete-line complete-line last))
                    sort)
        score-count (count scores)
        half-count (long (/ score-count 2))]
    (->> scores (drop half-count) first)))

(comment

  ;; pt 2

  (runner-pt2 ["[({(<(())[]>[[{[]{<()<>>"
               "[(()[<>])]({[<{<<[]>>("
               "{([(<{}[<>[]}>{[]{[(<()>"
               "(((({<>}<{<{<>}{[]{[]{}"
               "[[<[([]))<([[{}[[()]]]"
               "[{[{({}]{}}([{[{{{}}([]"
               "{<[[]]>}<{[{[{[]{()[[[]"
               "[<(<(<(<{}))><([]([]()"
               "<{([([[(<>()){}]>(<<{{"
               "<{([{{}}[<[[[<>{}]]]>[]]"]) ;; 288957

  (with-open [r (io/reader (io/resource "aoc-2021/day10.txt"))]
    (runner-pt2 (line-seq r))) ;; 1118976874

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
  (complete-line [\{ \(])
  (score-complete-line [\] \) \} \>])



  )
