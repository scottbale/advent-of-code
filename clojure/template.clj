(ns aoc-rename.me
  "Namespace docstring - don't forget to delete unused requires"
  (:require
   [aoc-2020.day2 :refer [nm]]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [debugger :refer [dbg]]
   [the.parsatron :as p]))


(defn runner
  "Runner docstring"
  [input]
  )

(comment

  ;; Run with test input from the puzzle description
  (runner [199
           200
           208
           263])

  ;; Run with the real input, get the final answer
  ;; Note `edn/read-string` only needed when input contains numbers
  (with-open [r (io/reader (io/resource "aoc-rename/me.txt"))]
    (runner (map edn/read-string (line-seq r))))


  )
