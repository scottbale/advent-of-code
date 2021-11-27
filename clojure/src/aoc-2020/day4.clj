(ns aoc-2020.day4
  (:require
   [clojure.java.io :as io]
   [debugger :refer [dbg]]
   [the.parsatron :as p]))

(p/defparser default []
  (p/let->> [v (p/many1 (p/choice (p/letter)
                                  (p/digit)
                                  (p/char \#)))]
            (p/always (apply str v))))

(p/defparser year []
  (p/let->> [v (p/times 4 (p/digit))]
            (p/always (Long/parseLong (apply str v)))))

(p/defparser height []
  (p/let->> [h (p/many (p/digit))
             unit (p/choice (p/string "in")
                             (p/string "cm")
                             (p/always nil))]
            (let [m {:h (->> h (apply str) (Long/parseLong))}]
              (p/always (if unit (assoc m :unit unit) m)))))

(def field-parsers
  {:hgt height
   :byr year
   :iyr year
   :eyr year})

(p/defparser passport-field []
  (p/let->> [k (p/many1 (p/letter))
             _ (p/char \:)]
            (p/always (apply str k))))

(p/defparser passport-field-value [k]
  (p/let->> [v ((get field-parsers k default))
             _ (p/either (p/char \ )
                         (p/eof))]
            (p/always v)))

(p/defparser passport-field-map []
  (p/let->> [k (passport-field)
             v (passport-field-value (keyword k))]
            (p/always {(keyword k) v})))

(p/defparser passport-fields []
  (p/let->> [ms (p/many (passport-field-map))]
            (p/always (apply merge ms))))

(defn nm
  "Read and return a number, or nil, from the input"
  [input]
  (try
    (Long/parseLong input)
    (catch NumberFormatException e
        nil)))

(defn xform-input [input]
  (remove (comp empty? first) (partition-by empty? input)))

(defn parse-passport-lines
  "Takes a list of strings representing one passport, returns single map"
  [lines]
  (apply merge (map #(p/run (passport-fields) %) lines)))

(def required-fields #{:byr :iyr :eyr :hgt :hcl :ecl :pid})

(defn valid-hgt? [{:keys [h unit]}]
  (case unit
    "in" (and (<= 59 h) (>= 76 h))
    "cm" (and (<= 150 h) (>= 193 h))
    false))

(defn valid-year? [min-year max-year year]
  (and (<= min-year year) (>= max-year year)))

(def hcls #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \a \b \c \d \e \f})

(defn valid-hcl? [[h & cl :as hcl]]
  (and
   (= h \#)
   (= 6 (count cl))
   (every? hcls cl)))

(def eye-colors #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(defn valid-ecl? [ecl]
  (eye-colors ecl))

(defn valid-pid? [pid]
  (and
   (= 9 (count pid))
   (nm pid)))

(def field-validators
  {:hgt valid-hgt?
   :byr (partial valid-year? 1920 2002)
   :iyr (partial valid-year? 2010 2020)
   :eyr (partial valid-year? 2020 2030)
   :hcl valid-hcl?
   :ecl valid-ecl?
   :pid valid-pid?})

(defn valid-passport? [m]
  (and
   (every? (set (keys m)) required-fields)
   (every? identity (map (fn [[k v]] ((get field-validators k (constantly true)) v)) m))))

(defn runner [input]
  (count (filter valid-passport? (map parse-passport-lines (xform-input input)))))

(comment

  (valid-hcl? "#123abc")
  (valid-hcl? "#123abz")
  (valid-hcl? "#123a")
  (valid-hcl? "123abc")
  (valid-pid? "001234828")
  (valid-pid? "0012348289")
  (valid-pid? "001x48289")

  (p/run (height) "60in")
  (p/run (height) "60cm")
  (p/run (height) "60foo")
  (p/run (height) "60")
  (p/run (year) "2112")

  (runner ["ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
           "byr:1937 iyr:2017 cid:147 hgt:183cm"
           ""
           "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
           "hcl:#cfa07d byr:1929"
           ""
           "hcl:#ae17e1 iyr:2013"
           "eyr:2024"
           "ecl:brn pid:760753108 byr:1931"
           "hgt:179cm"
           ""
           "hcl:#cfa07d eyr:2025 pid:166559648"
           "iyr:2011 ecl:brn hgt:59in"])

  #_({:ecl "gry", :pid "860033327", :eyr "2020", :hcl "#fffffd", :byr "1937", :iyr "2017", :cid "147", :hgt "183cm"} 
     {:iyr "2013", :ecl "amb", :cid "350", :eyr "2023", :pid "028048884", :hcl "#cfa07d", :byr "1929"} 
     {:hcl "#ae17e1", :iyr "2013", :eyr "2024", :ecl "brn", :pid "760753108", :byr "1931", :hgt "179cm"} 
     {:hcl "#cfa07d", :eyr "2025", :pid "166559648", :iyr "2011", :ecl "brn", :hgt "59in"})

  (with-open [r (io/reader (io/resource "aoc-2020/day4.txt"))]
    (runner (line-seq r))) ;147 ;213 ;255 too high

  (p/run (passport-field) "iyr:")
  (p/run (passport-field-value :hgt) "23in")
  (valid-hgt? {:h 60, :unit "in"})
  (valid-hgt? {:h 2112, :unit "cm"})
  (valid-hgt? {:h 23})
  (p/run (passport-field-value :hcl) "#cfa07d ")
  (p/run (passport-field-value :hcl) "#cfa07d")
  (p/run (passport-field-map) "iyr:2011 ecl:brn hgt:59in")
  (p/run (passport-fields) "iyr:2011 ecl:brn hgt:59in")
  (parse-passport-lines (list "iyr:2011 ecl:brn hgt:59in" "hcl:#cfa07d byr:1929"))
  (valid-passport? {:hcl "#cfa07d", :eyr "2025", :pid "166559648", :iyr "2011", :ecl "brn", :hgt "59in" :byr "foo"})
  (valid-passport? {:hcl "#cfa07d", :eyr "2025", :pid "166559648", :iyr "2011", :ecl "brn", :hgt "59in"})

  (apply merge (list {:iyr "2011"} {:ecl "brn"} {:hgt "59in"}))

  (select-keys {:hcl "#cfa07d", :eyr "2025", :pid "166559648", :iyr "2011", :ecl "brn", :hgt "59in"} required-fields)
  (select-keys {:hcl "#cfa07d", :eyr "2025", :pid "166559648", :iyr "2011", :ecl "brn", :hgt "59in"} required-fields)

  (every? #{:hgt :byr :foo} required-fields)
  (every? #{:byr :iyr :eyr :hgt :hcl :ecl :pid} required-fields)
  (every? #{:byr :iyr :eyr :hgt :hcl :ecl :pid :cid} required-fields)
  (every? 
   (set (keys {:iyr "2013", :ecl "amb", :cid "350", :eyr "2023", :pid "028048884", :hcl "#cfa07d", :byr "1929"})) 
   required-fields)

  (required-fields :foo)

  )
