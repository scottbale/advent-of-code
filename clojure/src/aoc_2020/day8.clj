(ns aoc-2020.day8
  (:require
   [aoc-2020.day2 :refer [nm]]
   [clojure.java.io :as io]
   [debugger :refer [dbg]]
   [the.parsatron :as p]))

(defn acc [arg state]
  (-> state
      (update :accumulator + arg)
      (update :counter inc)))

(defn jmp [arg state]
  (update state :counter + arg))

(defn nop [_ state]
  (update state :counter inc))

(def instrs {:acc acc
             :jmp jmp
             :nop nop})

(defn execute-next
  "Given the current program state and the program, execute the next instruction and return
  an updated state"
  [{:keys [counter] :as state} program]
  (let [[inst arg] (nth program counter)]
    ((instrs inst) arg state)))

(defn remember-inst
  "Remember (add to the 'history' coll) the current value of counter, which is the instruction that is
  to be executed next"
  [{:keys [counter] :as state}]
  (-> state
      (update :history conj counter)
      (update :seen conj counter)))

(defn continue?
  "Predicate, returns true IFF the program counter isn't already in the history"
  [{:keys [counter seen] :as state}]
  (not (contains? seen counter)))

(defn run-program
  "Run program to 'completion', return completed program state"
  [state program]
  (let [program-size (count program)]
    (letfn [(step [{:keys [accumulator counter] :as state}]
              (if (<= program-size counter) 
                state ;; successful termination
                (let [state' (-> state (remember-inst) (execute-next program))]
                  (if (continue? state') ;; check whether infinite loop detected
                    (recur state')
                    state'))))]
      (step state))))

(defn fresh-state [] 
  {:accumulator 0
   :counter 0
   :history []
   :seen #{}})

(p/defparser parse-inst []
  (p/let->> [inst-chars (p/times 3 (p/any-char))
             _ (p/char \ )
             plus-or-minus (p/either (p/char \+) (p/char \-))
             digits (p/many1 (p/digit))]
            (let [inst (keyword (apply str inst-chars))
                  arg (nm digits)
                  arg (if (= \- plus-or-minus)
                        (unchecked-negate arg)
                        arg)]
              (p/always [inst arg]))))

(defn flip-inst
  "If inst is a 'nop' or 'jmp', invert it"
  [[inst arg :as inst-pair]]
  (case inst
    :nop [:jmp arg]
    :jmp [:nop arg]
    inst-pair))

(defn tweak-program
  "Return a new program by flipping the instruction at index n of the given program"
  [program n]
  (concat
   (take n program)
   (conj
    (nthrest program (inc n))
    (flip-inst (nth program n)))))

(defn debug-program
  "Run program, return program state. If termination is abnormal, change one instruction and try again"
  [state program]
  (let [program-size (count program)]
    (letfn [(step [index-history]
              (let [flip-index (last index-history)
                    p' (if (nil? flip-index) program (tweak-program program flip-index))
                    {:keys [history counter] :as new-state} (run-program state p')]
                (if (= counter program-size)
                  new-state ;; successful termination
                  (recur (or (butlast index-history) history)))))]
      (step nil))))

(defn runner [input]
  (let [parser (parse-inst)
        program (map (partial p/run parser) input)]
    (:accumulator (debug-program (fresh-state) program))))



(comment

  (tweak-program [[:nop 1]
                  [:jmp 55]
                  [:acc 3]
                  [:jmp -2]
                  [:acc 2112]] 3)

  (p/run (parse-inst) "acc -4")

  (execute-next
   {:accumulator 3
    :counter 1}
   [[:nop :foo]
    [:acc 3]])

  (run-program
   (fresh-state)
   [[:nop 1]
    [:acc 3]
    [:jmp -2]])

  (runner ["nop +0"
           "acc +1"
           "jmp +4"
           "acc +3"
           "jmp -3"
           "acc -99"
           "acc +1"
           "jmp -4"
           "acc +6"])

  (with-open [r (io/reader (io/resource "aoc-2020/day8.txt"))]
    (runner (line-seq r))) ;1056


  )
