(ns advent-of-code.day3
  (:require [advent-of-code.common :as common]
            [clojure.string :as str]
            [clojure.set :as set]))

(def number "\\d+")
(def number-pattern #"\d+")
(def none "\\.+")
(def none-pattern #"\.+")
(def part "[^\\d\\.]")
(def part-pattern #"[^\d\.]")
(def schema-pattern (re-pattern (str/join "|" [number part none])))
(def puzzle-input (common/file->string-list "day3.txt"))

(defn create-token
  [t value]
  {:type  t
   :value (if (= t :number)
            (Integer. value)
            value)
   :count (count value)})

(defn lexer
  [s]
  (condp re-matches s
    number-pattern (create-token :number s)
    part-pattern   (create-token :part   s)
    none-pattern   (create-token :none   s)))

(defn parse-schematic
  [sc]
  (->> (mapv #(into [] (re-seq schema-pattern %)) sc)
       (mapv #(mapv lexer %))
       (map-indexed (fn [i row] (map #(assoc % :row i) row)))
       (mapv #(reduce (fn [acc curr] (if-let [t (last acc)]
                                      (conj acc (assoc curr :pos (+ (:pos t) (:count t))))
                                      (conj acc (assoc curr :pos 0))))
                      [] %))
       flatten))

(defn within-range?
  [s1 e1 s2 e2]
  (not-empty (set/intersection
              (into #{} (range (dec s1) (+ e1 2)))
              (into #{} (range s2 (inc e2))))))

(defn row-neighbour?
  [row1 row2]
  (<= (dec row1) row2 (inc row1)))

(defn get-neighbour-number-tokens
  [token number-tokens]
  (let [row        (:row token)
        start      (:pos token)
        end        (dec (+ start (:count token)))
        neighbours (filter #(and (row-neighbour? row (:row %))
                                 (within-range? start end (:pos %) (dec (+ (:pos %) (:count %)))))
                           number-tokens)]
    (into '() neighbours)))

(def first-start
  (let [parsed-schema (filter #(not= :none   (:type %)) (parse-schematic puzzle-input))
        numbers       (filter #(=    :number (:type %)) parsed-schema)
        parts         (filter #(=    :part   (:type %)) parsed-schema)]
    (->> (map (fn [part] (get-neighbour-number-tokens part numbers)) parts)
         flatten
         (into #{})
         (map :value)
         (apply +))))

(def second-star
  (let [parsed-schema (filter #(not= :none   (:type %))  (parse-schematic puzzle-input))
        numbers       (filter #(=    :number (:type %))  parsed-schema)
        gears         (filter #(=    "*"     (:value %)) parsed-schema)]
    (->> (map (fn [gear] (get-neighbour-number-tokens gear numbers)) gears)
         (filter #(< 1 (count %)))
         (map #(map (fn [g] (:value g)) %))
         (map #(apply * %))
         flatten
         (apply +))))
