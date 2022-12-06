(ns user
  (:require
   [clojure.java.io :as io]))

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
