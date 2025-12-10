(ns aoc-2025.day6
  "Cephalopod math homework in trash compactor"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as s]
   [debugger :refer [dbg]]))

(defn runner
  "build up a list of lists from the input"
  [input]
  (let [raw-list-of-lists
        (->>
         input
         reverse
         (map #(s/split % #" "))
         (map #(remove s/blank? %)))
        zipped-list-of-lists (apply map list raw-list-of-lists)]
    (->>
     zipped-list-of-lists
     (map (fn [l] (->> l (map edn/read-string) eval)))
     (reduce + 0))))



(comment

  (runner ["123 328  51 64"
           "45 64  387 23"
           "6 98  215 314"
           "*   +   *   +"]) ;; 4277556

  (-> "+" edn/read-string eval)
  (eval '(+ 1 2))

  (->>
   '("+" "1" "2")
   (map edn/read-string)
   eval)

  (with-open [r (io/reader (io/resource "aoc-2025/day6.txt"))]
    (runner (line-seq r))) ;; 7098065460541

  ;; end comment
  )
