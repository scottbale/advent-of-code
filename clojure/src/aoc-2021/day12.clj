(ns aoc-2021.day12
  "Cave / graph traversal"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as s]
   [debugger :refer [dbg]]))

(defn parse-line
  "Parse a line of input into a pair of String node names"
  [l]
  (s/split l #"-"))

(defn adjacency-map
  "Given sequence of node pairs, build an adjacency map where keys are node names and values are list
  of all nodes reachable from that node (except 'start' node)"
  [node-pairs]
  (letfn [(maybe-update [m n1 n2]
            ;; don't add "start" to any lists
            (if (or (= "end" n1) (= "start" n2))
              m
              (update m n1 conj n2)))]
    (reduce
     (fn [m [n1 n2]]
       (-> m
           (maybe-update n1 n2)
           (maybe-update n2 n1)))
     {} node-pairs)))

(defn find-routes
  "Given the adjacency map, recursively find and return the list of all routes from 'start' to 'end'
  that don't visit a lowercase cave more than once."
  [adjacency-map]
  (letfn [(complete? [partial-route]
            (= "end" (last partial-route)))
          (upper? [n]
            (Character/isUpperCase (first n)))
          (keep? [partial-route candidate-node]
            ;; only keep candidate new route if candidate-node is uppercase or not already in the route
            (or (upper? candidate-node) (nil? (some #{candidate-node} partial-route))))
          (route-goal [partial-route]
            ;; given partial-route, return coll of one or more longer routes
            (let [n (last partial-route)
                  nodes (adjacency-map n)]
              (reduce (fn [keep n]
                        (if (keep? partial-route n)
                          (conj keep (conj partial-route n))
                          keep)) #{} nodes)))
          (step [complete-routes [r & more :as partial-routes]]
            (if r
              (if (complete? r)
                (recur (conj complete-routes r) more)
                (recur complete-routes (concat more (route-goal r))))
              complete-routes))]
    (step #{} #{["start"]})))

(defn runner-pt1
  "Find the count of all routes that don't visit small cave more than once."
  [input]
  (->> input
       (map parse-line)
       adjacency-map
       find-routes
       count))


(comment

  ;; pt 1

  (runner-pt1 ["start-A"
               "start-b"
               "A-c"
               "A-b"
               "b-d"
               "A-end"
               "b-end"]) ;; 10
  ;;     start
  ;;     /   \
  ;; c--A-----b--d
  ;;     \   /
  ;;      end

  (with-open [r (io/reader (io/resource "aoc-2021/day12.txt"))]
    (runner-pt1 (line-seq r))) ;; 3298


  (parse-line "start-A")
  (adjacency-map [["start" "A"] ["start" "b"]])

  (find-routes {"start" '("b" "A"), "A" '("end" "b" "c"), "c" '("A"), "b" '("end" "d" "A"), "d" '("b")})

  (some #{4} [1 2 3])


  )
