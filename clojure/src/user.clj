(ns user
  (:require
   [clojure.java.io :as io]
   [clojure.string :as s]))

(defn newday
  "Generate a new namespace and data resource file for the next day's puzzle.
  e.g. (newday \"2022\" \"13\") will create a ns `aoc-2022.day13` and a resource file for the input
  data."
  [year-str day-str]
  (let [template (slurp (io/resource "template.clj.txt"))
        ns-year-str (str "aoc-" year-str)
        ns-day-str (str "day" day-str)
        final (format template ns-year-str ns-day-str ns-year-str ns-day-str)
        src-file (io/file "src" (str "aoc_" year-str) (str ns-day-str ".clj"))
        resource-file (io/file "resources" ns-year-str (str ns-day-str ".txt"))]
    (io/make-parents src-file)
    (spit src-file final)
    (io/make-parents resource-file)
    (spit resource-file "")
    (format "Created '%s' and '%s'" src-file resource-file)))


(comment ;; fun with transducers

  (def input
    ["$ cd /"
     "$ ls"
     "dir a"
     "14848514 b.txt"
     "8504156 c.dat"
     "dir d"
     "$ cd a"
     "$ ls"
     "dir e"
     "29116 f"
     "2557 g"
     "62596 h.lst"
     "$ cd e"
     "$ ls"
     "584 i"
     "$ cd .."
     "$ cd .."
     "$ cd d"
     "$ ls"
     "4060174 j"
     "8033020 d.log"
     "5626152 d.ext"
     "7214296 k"])

  (def xform-split-instruction
    (fn [f]
      (fn
        ([] (f))
        ([m] m)
        ([m x]
         (f m (s/split x #" "))))))

  (def xform-split-instruction
    (map #(s/split % #" ")))

  (def xform-count
    (fn [f]
      (completing
       (fn [m x]
         (f m (count x))))))

  (def xform-count
    (map count))

  (def xform-filter-shell-commands
    (fn [f]
      (completing
       (fn [m [a :as x]]
         (if (= "$" a)
           (f m x)
           m)))))

  (def xform-filter-shell-commands
    (filter (comp (partial = "$") first)))

  (def xform-extract-command
    (fn [f]
      (completing
       (fn [m [_ b]]
         (f m b)))))

  (def xform-extract-command
    (map second))

  (reduce str input)

  (reduce #(+ %1 (count %2)) 0 input)
  ;; (transduce xform2 str input)
  ;; (transduce xform2 (completing #(+ %1 (count %2))) 0 input)

  ;; count total number of input instruction pieces
  (transduce (comp xform-split-instruction xform-count) + input)

  ;; count total number of shell command input instruction pieces
  (transduce (comp xform-split-instruction xform-filter-shell-commands xform-count) + input)

  ;; see only the shell command instructions
  (sequence (comp xform-split-instruction xform-filter-shell-commands xform-extract-command) input)

  ;; see only the shell command instructions (non-transducer)
  (->> input
       (map #(s/split % #" "))
       (filter (comp (partial = "$") first))
       (map second))

  ;; transducers again
  (->> input
       (sequence
        (comp
         (map #(s/split % #" "))
         (filter (comp (partial = "$") first))
         (map second))))

  ;; test `transduce` that "if coll contains no items, return init and f is not called"
  (transduce (map name) (fn
                          ([m] (println "called") nil)
                          ([m _] (println "calledd") m)) 0 [])

  ;;end comment
  )
