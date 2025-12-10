(ns aoc-2025.day6
  "Cephalopod math homework in trash compactor"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as s]))

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

(defn lists-of-chars->number
  "This fn accepts a sequence of lists of chars and returns a number which is the
  evaluation of the form built from the operator and numbers encoded in the
  chars.

  Example:

   ((\\1 \\space \\space \\*)
    (\\2 \\4 \\space \\space)
    (\\3 \\5 \\6 \\space))

  This is transformed into the form

  (* 1 24 356)

  Which the function evaluates and returns. "
  [[l & ls]]
  (let [operator-char (last l)
        form (list (-> operator-char str edn/read-string eval))
        remaining (cons (drop-last l) ls)
        char-list->num (fn [l] (->> l (apply str) s/trim edn/read-string))]
    (eval (concat form (map char-list->num remaining)))))

(defn runner2
  "From the input, construct the math operations (addition or multiplication) and
  operands, evaluate them, and compute their sum."
  [input]
  (let [raw-list-of-lists-of-chars
        ;; transform every line of input into a sequence of chars
        (map (fn [line-of-input] (map identity line-of-input)) input)
        list-of-lists-of-chars
        ;; 'vertical lists': group into lists the 1st, 2nd, etc., chars
        (apply map list raw-list-of-lists-of-chars)]
    (->>
     (loop [l-of-l list-of-lists-of-chars
            nums []]
       ;; Each list of only spaces is a separator of expressions.
       ;; Each loop iteration handles one expression in between the separator(s).
       (let [next-num (take-while (fn [l] (not (->> l (apply str) s/blank?))) l-of-l)
             nm (lists-of-chars->number next-num)
             nums (conj nums nm)
             remaining (drop (inc (count next-num)) l-of-l)]
         (if (seq remaining)
           (recur remaining nums)
           nums)))
     (reduce + 0))))

(comment

  (lists-of-chars->number
   '((\1 \space \space \*)
     (\2 \4 \space \space)
     (\3 \5 \6 \space))) ;; 8544

  (runner2 ["123 328  51 64 "
            " 45 64  387 23 "
            "  6 98  215 314"
            "*   +   *   +  "]) ;; 3263827

  #_((\1 \space \space \*)
     (\2 \4 \space \space)
     (\3 \5 \6 \space)
     (\space \space \space \space)
     (\3 \6 \9 \+)
     (\2 \4 \8 \space)
     (\8 \space \space \space)
     (\space \space \space \space)
     (\space \3 \2 \*)
     (\5 \8 \1 \space)
     (\1 \7 \5 \space)
     (\space \space \space \space)
     (\6 \2 \3 \+)
     (\4 \3 \1 \space)
     (\space \space \4 \space)
     )

  (runner ["123 328  51 64"
           " 45 64  387 23"
           "  6 98  215 314"
           "*   +   *   +"]) ;; 4277556

  (-> "+" edn/read-string eval)
  (eval '(+ 1 2))

  (->>
   '("+" "1" "2")
   (map edn/read-string)
   eval)

  (with-open [r (io/reader (io/resource "aoc-2025/day6.txt"))]
    (runner (line-seq r))) ;; 7098065460541

  (with-open [r (io/reader (io/resource "aoc-2025/day6.txt"))]
    (runner2 (line-seq r))) ;; 13807151830618

  ;; end comment
  )
