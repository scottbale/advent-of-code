(ns aoc-2021.day7
  "Crabs in submarines"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [debugger :refer [dbg]]))

(defn runner-pt1
  [input]
  (let [input (edn/read-string (str \[ input \]))
        input (sort input)
        total (reduce + input)
        size (count input)
        avg (/ total size)
        _ (println ">>>>>> total, size, avg" total size avg)
        c (long (Math/ceil avg))
        f (long (Math/floor avg))
        _ (println ">>>> ceil floor" c f)
        increasing (range (inc c) (inc (last input)))
        decreasing (range f (dec (first input)) -1)
        _ (println ">>>> endpoints" (first input) (last input))
        _ (println ">>>> endpoints increasing" (first increasing) (last increasing))
        _ (println ">>>> endpoints decreasing" (first decreasing) (last decreasing))
        
        
        summer (fn [n]
                 (reduce + (map (fn [j] (Math/abs (- n j))) input)))
        
        sum-c (summer c)
        minner (fn [[res sum] n]
                 (let [sum' (summer n)]
                   (println ">>>>>comparing" [res sum] [n sum'] "for" res n)
                   (if (< sum sum')
                     (reduced [res sum])
                     [n sum'])))
        [x sum-x] (reduce minner [c sum-c] increasing)
        [x sum-x] (reduce minner [x sum-x] decreasing)]
    sum-x
    ))

(comment

  (runner-pt1 "16,1,2,0,4,2,7,1,2,14")

  (with-open [r (io/reader (io/resource "aoc-2021/day7.txt"))]
    (runner-pt1 (first (line-seq r)))) ;; 355150

  ;; scratchwork

  (let [input "16,1,2,0,4,2,7,1,2,14"
        input (edn/read-string (str \[ input \]))
        input (sort input)
        search (into #{} (range (first input) (inc (last input))))
        f (fn [m i]
            (assoc m i (reduce + (map (fn [j] (Math/abs (- i j))) input))))
        results (reduce f {} search)
        avg (/ (reduce + input) (count input))]
    (clojure.pprint/pprint 
     {:m (into (sorted-map) results)
      :avg avg})
    )

  (let [input "16,1,2,0,4,2,7,1,2,14"
        input (edn/read-string (str \[ input \]))
        input (sort input)
        avg (/ (reduce + input) (count input))
        c (long (Math/ceil avg))
        f (long (Math/floor avg))
        increasing (range c (inc (last input)))
        decreasing (range f (dec (first input)) -1)
        
        summer (fn [n]
                 (reduce + (map (fn [j] (Math/abs (- n j))) input)))
        
        sum-c (summer c)
        minner (fn [[res sum] n]
                 (let [sum' (summer n)]
                   (println ">>>>>comparing" [res sum] [n sum'] "for" res n)
                   (if (< sum sum')
                     (reduced [res sum])
                     [n sum'])))
        [x sum-x] (reduce minner [c sum-c] increasing)
        [x sum-x] (reduce minner [x sum-x] decreasing)]
    x
    )




  )
