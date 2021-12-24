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

(defn complete? [partial-route]
  (= "end" (last partial-route)))

(defn upper? [n]
  (Character/isUpperCase (first n)))

(defn keep?
  "only keep candidate new route if candidate-node is uppercase or not already in the route"
  [partial-route candidate-node]
  (or (upper? candidate-node) (nil? (some #{candidate-node} partial-route))))

(defn permit-small-cave-twice?
  "Return true if no small cave has yet been visited twice"
  [partial-route]
  (->> (frequencies partial-route)
       (filter (fn [[k v]] (and (not (upper? k)) (== 2 v))))
       count
       zero?))

(defn keep-pt2?
  "Like `keep?`, but allow routes which visit one small cave twice"
  [partial-route candidate-node]
  (or
   (upper? candidate-node)
   (nil? (some #{candidate-node} partial-route))
   (permit-small-cave-twice? partial-route)))

(defn find-routes
  "Given the adjacency map, recursively find and return the list of all routes from 'start' to 'end'.
  `keep-fn` accepts a partial route (seq of strings) and a candidate node (String) and decides
  whether the new resulting route is valid or not. "
  [keep-fn adjacency-map]
  (letfn [(route-goal [partial-route]
            ;; given partial-route, return coll of one or more longer routes
            (let [n (last partial-route)
                  nodes (adjacency-map n)]
              (reduce (fn [keep n]
                        (if (keep-fn partial-route n)
                          (conj keep (conj partial-route n))
                          keep)) #{} nodes)))
          (step [complete-routes [r & more :as partial-routes]]
            (if r
              (if (complete? r)
                (recur (conj complete-routes r) more)
                ;; `into` rather than `concat` - much faster and prevents stackoverflow in part 2
                ;; thank you once again https://stuartsierra.com/2015/04/26/clojure-donts-concat
                (recur complete-routes (into more (route-goal r))))
              complete-routes))]
    (step #{} #{["start"]})))

(defn runner-pt1
  "Find the count of all routes that don't visit small cave more than once."
  [input]
  (->> input
       (map parse-line)
       adjacency-map
       (find-routes keep?)
       count))

(defn runner-pt2
  "Like part 1, except additionally include routes where one small cave can be visited twice per
  route."
  [input]
  (->> input
       (map parse-line)
       adjacency-map
       (find-routes keep-pt2?)
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

  (time (with-open [r (io/reader (io/resource "aoc-2021/day12.txt"))]
          (runner-pt1 (line-seq r)))) ;; 3298 "Elapsed time: 192.857966 msecs"


  ;; pt 2

  (runner-pt2 ["start-A"
               "start-b"
               "A-c"
               "A-b"
               "b-d"
               "A-end"
               "b-end"]) ;; 36

  (time (with-open [r (io/reader (io/resource "aoc-2021/day12.txt"))]
          (runner-pt2 (line-seq r)))) ;; 93572 "Elapsed time: 27915.674023 msecs"


  (parse-line "start-A")
  (adjacency-map [["start" "A"] ["start" "b"]])

  (find-routes {"start" '("b" "A"), "A" '("end" "b" "c"), "c" '("A"), "b" '("end" "d" "A"), "d" '("b")})

  #{["start" "A" "c" "A" "b" "end"] ["start" "A" "c" "A" "b" "A" "end"] ["start" "b" "A" "end"] ["start" "b" "A" "c" "A" "end"] ["start" "A" "end"] ["start" "b" "end"] ["start" "A" "b" "end"] ["start" "A" "b" "A" "c" "A" "end"] ["start" "A" "b" "A" "end"] ["start" "A" "c" "A" "end"]}

  (some #{4} [1 2 3])

  (permit-small-cave-twice? ["start" "A" "c" "A" "b" "end"])

  (->> {"start" 1, "A" 2, "c" 2, "b" 1, "end" 1}
       (filter (fn [[k v]] (and (not (upper? k)) (== 2 v)))))

  )
