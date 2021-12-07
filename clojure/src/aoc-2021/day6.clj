(ns aoc-2021.day6
  "Modeling Lanternfish exponential growth"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [debugger :refer [dbg]]))

(defn runner-pt1
  ""
  [input day]
  (letfn [(spawndays [timer begin-day target-day]
            (range (inc (+ begin-day timer)) (inc target-day) 7))
          (total [day running-total [spawnday & more-days]]
            ;; (println ">>>>>" safety spawnday more-days)
            ;; recursively count all fish by day 'day'
            ;; At each step:
            ;; * If queue is empty, return the running total
            ;; * take next spawn day 'd' from queue - it represents a new fish
            ;; * recur, increment running total, generate spawndays for 'd', append to rest of queue
            (if (nil? spawnday)
              running-total
              (recur day (inc running-total) (concat more-days (spawndays 8 spawnday day)))))
          ;; Start the recursive step:
          ;; * initial total of one (for the initial fish)
          ;; * initial queue of that fish's spawn days (not yet totaled)
          (totaler [timer] (total day 1 (spawndays timer 0 day)))]
    (let [input (edn/read-string (str \[ input \]))]
      (reduce + (map totaler input)))))

(comment

  ;; pt 1

  (runner-pt1 "3,4,3,1,2" 80)

  (with-open [r (io/reader (io/resource "aoc-2021/day6.txt"))]
    (runner-pt1 (first (line-seq r)) 80)) ;; 358214

  ;; -------------------------------------------------------------------------------------------
  ;; Below this line is much doodling
  ;; ===========================================================================================

  ;; double every 7 days on avg
  ;; y = f(t) = 2^(t%7)
  ;; 

  (let [t 1
        e (inc (mod t 7))]
    e)


  (map #(- % (mod % 7) -1) [0 1 2 3 4 5 6 7 8 9 10])
  (map #(inc (Math/floorDiv % 7)) [0 1 2 3 4 5 6 7 8 9 10])

  (let [foo (fn [t] (inc (Math/floorDiv t 7)))]
    (foo 80))


  ;; Count of fish on day t given single initial fish = 
  ;;   sum of that fish plus every fish it spawns in that many days +
  ;;   recursively (for each spawned fish) the same calculation for t-? days
  ;; 
  ;;  t: 1 2 3 4 5 6 7 8 9 10 11 12 13 
  ;;  x: 4

  ;; treat every fish as newborn at first; initial condition fish are offset the initial 8 days by N days
  (let [day 18
        input [3,4,3,1,2]]
    (letfn [(fishcount [timer-offset days] 
              (let [adjusted-days (- (+ days 8) timer-offset)]
                (cond
                  (< adjusted-days 8) 1
                  :else (inc (Math/floorDiv (- adjusted-days 2) 7)))))
            (spawndays [timer-offset days] ;;finite seq of day ints (each less than 'days')
              (let [adjusted-days (- (+ days 8) timer-offset)
                    adjuster #(- % (- 7 timer-offset))]
                (cond
                  (< adjusted-days 8) []
                  :else (map adjuster (range 8 adjusted-days 7))))
              )
            (fishcount2 [timer-offset days]
              (inc (count (spawndays timer-offset days))))
            (step [running-total [spawnday & more-spawndays :as spawndays-queue] safety]
              (if (== 0 (mod safety 1))
                (println ">>>>>>" safety ", total: " running-total "," spawndays-queue))
              (if (and (>= safety 0) spawnday)
                (let [spawnday' (- day spawnday)
                      spawndays' (spawndays 8 spawnday')]
                  (println ">>>>>>For next spawnday" spawnday "adjusted" spawnday' "yields" spawndays')
                  (recur (inc running-total) (concat more-spawndays spawndays') (dec safety)))
                running-total))]
      (let [offset 3]
        {:spawndays (spawndays offset day)
         :fishcount (fishcount2 offset day)
         :recursive-count (step 1 (spawndays offset day) 50)})
      ;; (step 1 (spawndays 3 day) 50)
      ));; {:spawndays (4 11 18), :fishcount 4, :recursive-count 52}


  (letfn [#_(timer->age [timer]
            (- 8 timer))
          #_(spawndays0 [day]
            ;; seq of days on which ready-to-spawn fish (timer=0 initially) would spawn
            (range 8 day 7))
          #_(spawndays [fish-initial-timer day]
            ;; seq of days on which indicated fish will spawn, including up through target day
            (let [offset (timer->age fish-initial-timer)
                  spawndays' (spawndays0 (+ offset day))
                  adjuster #(- % offset)]
              (map adjuster spawndays'))
            )
          (spawndays [timer begin-day target-day]
            (range (inc (+ begin-day timer)) (inc target-day) 7))
          #_(spawncount [timer day]
            ;; Count the fish, and fish it directly spawns, by day 'day'
            (inc (count (spawndays timer 0 day))))
          (total [day running-total [spawnday & more-days] safety]
            ;; (println ">>>>>" safety spawnday more-days)
            ;; recursively count all fish by day 'day'
            ;; At each step:
            ;; * If queue is empty, return the running total
            ;; * take next spawn day 'd' from queue - it represents a new fish
            ;; * recur, increment running total, generate spawndays for 'd', append to rest of queue
            (if (or (nil? spawnday) (== 0 safety))
              running-total
              (recur day (inc running-total) (concat more-days (spawndays 8 spawnday day)) (dec safety)))
            )
          ]
    (let [timer 3
          day 18
          init-day 0
          input [3,4,3,1,2]
          totaler (fn [timer] (total day 1 (spawndays timer 0 day) 20))]
      ;; (spawncount timer day)
      ;; Start the recursive step:
      ;; * initial total of one (for the initial fish)
      ;; * initial queue of that fish's spawn days (not yet totaled)
      ;; (total day 1 (spawndays timer 0 day) 20)
      ;; (spawndays 8 11 18)
      (reduce + (map totaler input)) ;; 35 too high should be 26
      #_{:timer timer
       :day day
       :init-day init-day
       :spawndays (spawndays timer init-day day)
       :total (totaler timer)}
      ))

  {:timer 3, :day 18, :init-day 0, :spawndays (4 11 18), :total 5}

  ;; {:timer 3, :day 18, :spawndays (3 10 17), :total 7}

  ;; at each step: running sum plus, consume next in queue of fishcounts per int days to calculate


  ;;  t|x,...
  ;; --------
  ;;  0|0
  ;;  1|6,8
  ;;  2|5,7
  ;;  ...
  ;;  7|0,2
  ;;  8|6,1,8
  ;;  9|5,0,7
  ;; 10|4,6,6,8
  ;;  ...
  ;; 14|0,2,2,4
  ;; 15|6,1,1,3,8
  ;; 16|5,0,0,2,7


  ;;  t|y,...        |age
  ;; --|-------------|----
  ;;  0|0            | 7
  ;;  1|6,       8   | 8,      0
  ;;  2|5,       7   | 9,      1
  ;;  3|4,       6   |10,      2
  ;;  ...
  ;;  7|0,       2   |14,      6
  ;;  8|6,   8,  1   |15,   0, 7
  ;;  9|5,   7,  0   |16,   1, 8
  ;; 10|4,   6,  6,8 |17,   2, 9, 0
  ;;  ...
  ;; 14|0,   2,  2,4 |21,   6, 13,4
  ;; 15|6,8, 1,  1,3
  ;; 16|5,7, 0,  0,2


  (letfn [(gen [n]
            (cond
              (== n 0) 1
              (<= n 8) 0
              :else (+ (gen (- n 9)) (gen (- n 7)))))
          (f' [t]
            (case t
              0 1
              (+ (schedule (dec t)) (gen t))))
          (schedule [t]
            (nth (map f' (range 100)) t))]
    (map gen (range 0 20))
    ;; (map schedule (range 0 3) )
    ;; (f' 1)
    )

  )
