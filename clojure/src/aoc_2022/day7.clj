(ns aoc-2022.day7
  "No Space Left On Device"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.zip :as z]
   ;; [debugger :refer [dbg]]
   ))

(defn z-total
  "Given a zipper data structure like [n [i ...] [j ...] ...]], return an updated zipper where i,j,...
  has been added to `n`. The invariant is: the first item in the vector is the total size of the
  current directory. There are zero or more subdirs (represented by nested vectors which each
  themselves conform to the invariant of having a total size as their first element)."
  [loc]
  (let [childloc (z/down loc)]
    (z/up (apply z/edit childloc + (map first (z/rights childloc))))))

(defn z-add
  "Adds size `n` to current dir"
  [loc n]
  (-> loc
      z/down
      (z/edit + n)
      (z/up)))

(defn z-dir
  "append an empty dir at `loc`"
  [loc]
  (-> loc
      (z/append-child [0])))

(defn z-step
  "reduce fn, takes zipper and current instruction, returns possibly updated zipper"
  [loc inst]
  (let [[a b c] (str/split inst #" ")]
    (cond
      ;; detect a file, add size
      (number? (edn/read-string a))
      (z-add loc (edn/read-string a))
      ;; detect a cd, either into a subdir or up
      (and (= "$" a) (= "cd" b))
      (if (= ".." c)
        (-> loc z-total z/up)
        (-> loc z-dir z/down z/rightmost))
      :else
      loc)))

(defn runner
  "runner docstring"
  [input]
  (->> input
       rest ;; ignore the first instruction `$ cd /`
       (reduce z-step (z/vector-zip [0]))
       (z/root)
       (tree-seq vector? seq)    
       (filter number?)
       (filter #(>= 100000 %))
       (reduce +)))


(comment

  (runner ["$ cd /"
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
           "7214296 k"]) ;; 95437

  (with-open [r (io/reader (io/resource "aoc-2022/day7.txt"))]
    (runner (line-seq r))) ;; 1555642

  (->> [48381165
        #_a [94853
             #_e [584]]
        #_d [24933642]]
       (tree-seq vector? seq)
       (filter number?)
       (filter #(>= 100000 %))
       (reduce +))

  (-> (z/vector-zip [0])
      (z-add 100)
      (z-dir)
      (z/down)
      (z/rightmost)
      (z-add 3)
      (z/up)
      (z-dir)
      (z/down)
      (z/rightmost)
      (z-add 6)
      (z/up)
      (z-total)
      z/root)

  (-> (z/vector-zip [0 [1] [14]])
      (z-add 3)
      (z-add 7)
      z/root)

  (-> (z/vector-zip [0 [1] [14]])
      z-total
      z/root)

  (-> (z/vector-zip [0 [0 [8] [3]] [14]])
      z/down
      z/right
      z-total
      z/up
      z-total
      z/root)

  (let [vz (z/vector-zip [0])
        total (fn [rs]
                (let [loc (z/down rs)]
                  (z/up (z/edit loc + (map first (z/rights loc))))))
        x (-> vz
            (z/down)
            (z/edit + 23)
            (z/edit + 13)
            (z/up)
            (z/append-child [0])
            (z/down)
            (z/rightmost)
            (z/down)
            (z/edit + 10)
            (z/up)
            (z/up)
            (z/append-child [0])
            (z/down)
            (z/rightmost)
            (z/down)
            (z/edit + 8)
            (z/up)
            (z/up)
            (z/down)
        ;; (z/rights)
        ;; (z/edit + [10 8])
        ;; (total)
        ;; (z/root)
            )]
    (z/root (apply z/edit x + [10 8])))

  ;; invariant: new node representing dir is [0] - zero initial size, zero subdirs
  ;; * "ls" 
  (let [vz (z/vector-zip [0])]
    (-> vz
        (z/down)
        (z/edit + 23)
        (z/edit + 13)
        (z/up)
        (z/append-child [0])
        (z/append-child [0])
        (z/down)
        (z/right)
        (z/down)
        (z/edit + 10)
        (z/up)
        (z/root)))

  (let [vz (z/vector-zip [[2] [9 [4 5]]])]
    (-> vz
        (z/down)
        ;; (z/root)
        ))

  (let [vz (z/vector-zip [])]
    (-> vz
        (z/insert-child 3)
        (z/append-child 4)
        (z/append-child 5)
        (z/down)
        ;; (z/root)
        ))

  (let [;; vz (z/zipper vector? seq (fn [_ c] c) [[3 4]])
        vz (z/vector-zip [1 2 [3 4]])]
    (-> vz

        (z/down)
        (z/right)
        ;; (z/prev)
        ))
  

  (->> [13 [8 [5] [3]] [5]]
       (tree-seq vector? seq)
       (filter number?)
       (remove #(>= 8 %))
       (reduce +))

  (tree-seq vector? seq [13 [8 [5] [3]] [5]])
  #_([13 [8 [5] [3]] [5]]
     13
     [8 [5] [3]]
     8
     [5]
     5
     [3]
     3
     [5]
     5)

  ;; recursive function
  ;; -return: total size of current directory
  ;; -params:
  ;; * 
  ;; * input-seq
  ;; At each step
  ;; * total sizes of any files (always the first val in a vector)
  ;; * recursively add any subdirs
  ;; * return a vector

  [])
