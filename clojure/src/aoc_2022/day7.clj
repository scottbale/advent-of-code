(ns aoc-2022.day7
  "No Space Left On Device"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [clojure.zip :as z]))

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
  (z/append-child loc [0]))

(defn z-step
  "reduce fn, takes zipper and current instruction, returns possibly updated zipper. Uses `reduced` to
  indicate completion, because the implementation reduces over an infinite sequence of instructions."
  [loc inst]
  (let [[a b c] (str/split inst #" ")]
    (cond
      ;; detect a file, add size
      (number? (edn/read-string a))
      (z-add loc (edn/read-string a))
      ;; detect a cd, either into a subdir or up
      (and (= "$" a) (= "cd" b))
      (if (= ".." c)
        ;; Already at the root of the tree?
        (if (nil? (z/up loc))
          ;; Finished, short-circuit reduction
          (reduced (z-total loc))
          ;; Else calculate dir total and navigate upward one level
          (-> loc z-total z/up))
        ;; Else append a new empty subdir and navigate down to it
        (-> loc z-dir z/down z/rightmost))
      :else
      loc)))

(defn z-runner
  "Given the input sequence, return a zipper structure that represents the filesystem's directory
  hierarchy and sizes. It will be a vector of longs and nested vectors.

  For example: `[117 [10 [5 2 3]] [7]]`

  The invariant is that the first element of every vector should be a simple number which is the
  total size of that (sub)directory. The size includes size(s) of file(s) in that dir (which are not
  otherwise represented) and size(s) of any nested subdir(s). In this example, the root dir has a
  size of 117, which is the sum of subdirs sizes (10 + 7) plus another 100 of files.

  One tricky bit is that the puzzle input does not necessarily `cd ..` back to the root of the
  filesystem tree. This implementation uses the `cd ..` to prompt calculation of the total of each
  directory, so it's necessary to conclude the traversal by `cd ..` back to the root. I've done that
  by concating a lazy infinite sequence of `$ cd ..` on to the end of the puzzle. But this means
  that the `reduce` has to be written to short-circuit, using `reduced`."
  [input]
  (->> (concat input (repeat "$ cd .."))
       rest ;; ignore the first instruction `$ cd /`
       (reduce z-step (z/vector-zip [0]))))

(defn runner1
  "Pt 1: find the sum of sizes of all subdirectories not more than 100k in size."
  [input]
  (->> input
       z-runner
       z/root
       (tree-seq vector? seq)
       (filter number?)
       (filter #(>= 100000 %))
       (reduce +)))

(defn runner
  "Pt 2: find size of the smallest directory that, if deleted, will free up enough disk space."
  [input]
  (let [fs 70000000
        needed 30000000
        z-root (z-runner input)
        root (z/root z-root)
        dirs (->> root
                  (tree-seq vector? seq)
                  (filter number?))
        used (first dirs)
        available (- fs used)
        target (- needed available)
        pred (fn [x] (when (<= target x) x))
        sorted-low-to-high (sort dirs)]
    (some pred sorted-low-to-high)))


(comment


  (->> (z-runner ["$ cd /"
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
       z/root
       walk/prewalk-demo)

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
           "7214296 k"])
  ;; 24933642 ;; 95437
  ;; full tree should be [48381165 [94853 [584]] [24933642]]

  (with-open [r (io/reader (io/resource "aoc-2022/day7.txt"))]
    (runner (line-seq r))) ;; 5974547 ;; 15584 too low ;; 1555642

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
      z/down
      z/up
      z/up
      ;;z/root
      )

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

  (let [ ;; vz (z/zipper vector? seq (fn [_ c] c) [[3 4]])
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
