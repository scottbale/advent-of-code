(ns aoc-2020.day2
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [debugger :refer [dbg]]
   [the.parsatron :as p]))

(defn nm [digits]
  (->> digits
       (apply str)
       (edn/read-string)))

(p/defparser password-policy-parser []
  (p/let->> [lowests (p/many1 (p/digit))
             _ (p/char \-)
             highests (p/many1 (p/digit))
             _ (p/char \ )
             letter (p/any-char)]
            (p/always {:lowest (nm lowests)
                       :highest (nm highests)
                       :letter letter})))

(p/defparser password-parser []
  (p/let->> [raw (p/many1 (p/any-char))]
            (p/always (apply str raw))))

(p/defparser both-parser []
  (p/let->> [ppolicy (password-policy-parser)
             _ (p/char \:)
             _ (p/char \ )
             pwd (password-parser)]
            (p/always [ppolicy pwd])))

(defn conforms? [{:keys [lowest highest letter]} password]
  (let [letter-count (get (frequencies password) letter 0)]
    (and (<= lowest letter-count) (>= highest letter-count))))

(defn conforms-pt2? [{:keys [lowest highest letter]} password]
  (== 1 (get (frequencies (list (nth password (dec lowest)) (nth password (dec highest)))) letter 0)))

(defn valid?
  "Parse input, indicate t/f if it's valid"
  [parser conforms-fn input]
  (let [[policy pwd] (p/run parser input)]
    (conforms-fn policy pwd)))

(comment

  (let [input ["1-3 a: abcde" 
               "1-3 b: cdefg" 
               "2-9 c: ccccccccc"]
        parser (both-parser)
        valid-fn (partial valid? parser conforms-pt2?)]
    (->> input
         (map valid-fn)
         (filter true?)
         count))

  (with-open [r (io/reader (io/resource "aoc-2020/day2.txt"))]
    (let [input (line-seq r)
          parser (both-parser)
          valid-fn (partial valid? parser conforms-pt2?)]
      (->> input
         (map valid-fn)
         (filter true?)
         count)))


  (p/run (both-parser) "12-13 a: abcde")

  (conforms? {:lowest 1, :highest 6, :letter \a} "abcdea")

  ;; 9-19 q: qqqqqqqqqqqqqqqqqqqq
  (conforms-pt2? {:lowest 9, :highest 19, :letter \q} "qqqqqqqqqqqqqqqqqqqq")

  ((frequencies "abcde") \f)

  
  )
