(ns aoc-2020.day7
  (:require
   [aoc-2020.day2 :refer [nm]]
   [clojure.java.io :as io]
   [debugger :refer [dbg]]
   [the.parsatron :as p]))

(p/defparser bag-subject [] ;; String name of color
  (p/let->> [adjective (p/many (p/letter))
             _ (p/char \ )
             color (p/many (p/letter))
             _ (p/string " bags ")]
            (p/always (str (apply str adjective) " " (apply str color)))))

(p/defparser bag [] ;; String name of color
  (p/let->> [adjective (p/many (p/letter))
             _ (p/char \ )
             color (p/many (p/letter))
             _ (p/string " bag")
             _ (p/either
                (p/char \s)
                (p/always nil))
             _ (p/choice
                (p/char \,)
                (p/char \.))
             _ (p/either
                (p/char \ )
                (p/eof))]
            (p/always (str (apply str adjective) " " (apply str color)))))

(p/defparser quantity-of-bags [] ;; Pair (number quantity, String color name)
  (p/let->> [digits (p/many1 (p/digit))
             _ (p/char \ )
             bag (bag)]
            (p/always [(nm digits) bag])))

(p/defparser contains-bags [] ;; Seq of color names
  (p/let->> [_ (p/string "contain ")
             bags (p/either
                   (p/string "no other bags.")
                   (p/many1 (quantity-of-bags)))]
            (p/always (if (seq? bags) bags (list)))))

(p/defparser bag-map [] ;; Consume whole line, return map color->Set of colors
  (p/let->> [color (bag-subject)
             colors (contains-bags)]
            (p/always {color (set colors)})))

(defn all-bags
  "Recursively traverse map, return seq of all colors reachable from 'color'"
  [color m]
  (loop [colors-queue #{color}
         results #{}]
    (if (seq colors-queue)
      (let [another-queue (set (mapcat m colors-queue))]
        (recur another-queue (concat results another-queue)))
      results)))

(defn all-bags-count
  "Recursively traverse map, return count of all bags contained by 'color' bag"
  [color m]
  (loop [qty-colors-pair-queue [[1 color]]
         result 0]
    (if (seq qty-colors-pair-queue)
      (let [another-queue (mapcat (comp m last) qty-colors-pair-queue)
            inc-result-by (reduce + 0 (map first qty-colors-pair-queue))]
        (recur another-queue (+ result inc-result-by)))
      result)))

(defn invert-map-of-sets [m]
  ;; copied from  https://clojuredocs.org/clojure.set/map-invert
  (reduce
   (fn [a [k v]] (assoc a k (conj (get a k #{}) v))) {}
   (for [[k s] m v s] [v k])))

(defn recursive-count
  "w/o loop/recur"
  [color color->bag-pairs]
  (letfn [(nested-count [bag-color]
            ;; return a count of the number of nested bags (don't count the fn parameter itself)
            (if-let [nested-bag-pairs (color->bag-pairs bag-color)]
              (reduce (fn [sum [nested-n nested-bag-color]]
                        (+ sum nested-n (* nested-n (nested-count nested-bag-color)))) 0 nested-bag-pairs)
              0))]
    (nested-count color)))

(defn runner [color input]
  (->> input
       (map (partial p/run (bag-map)))
       (apply merge)
       ;; invert-map-of-sets
       (recursive-count color)
       ;; set ;; <- meant for this to be done in all-bags, but laziness :|
       ;; count
       ))

(comment

  (runner
   "shiny gold"
   ["shiny gold bags contain 2 dark red bags."
    "dark red bags contain 2 dark orange bags."
    "dark orange bags contain 2 dark yellow bags."
    "dark yellow bags contain 2 dark green bags."
    "dark green bags contain 2 dark blue bags."
    "dark blue bags contain 2 dark violet bags."
    "dark violet bags contain no other bags."]) ;; 126

  (runner
   "shiny gold"
   ["light red bags contain 1 bright white bag, 2 muted yellow bags."
    "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
    "bright white bags contain 1 shiny gold bag."
    "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
    "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
    "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
    "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
    "faded blue bags contain no other bags."
    "dotted black bags contain no other bags."]) ;; 32

  (with-open [r (io/reader (io/resource "aoc-2020/day7.txt"))]
    (runner "shiny gold" (line-seq r))) ;; 10219

  (let [m {"shiny gold" #{"muted yellow" "bright white"}
           "bright white" #{"dark orange" "light red"}}]
    ;; (all-bags "shiny gold" m)
    ;; (invert-map-of-sets m)
    )

  (p/run (bag-map) "light red bags contain 1 bright white bag, 2 muted yellow bags.")
  (p/run (quantity-of-bags) "4 light red bags, ")
  (p/run (contains-bags) "contain 4 light red bags, 2 muted yellow bags.")
  (p/run (contains-bags) "contain no other bags.")
  (p/run (p/many (bag)) "light red bag, light blue bag.")
  (p/run (bag) "light red bag, ")
  (p/run (bag) "light red bags, ")
  (p/run (bag) "light red bags.")



  )
