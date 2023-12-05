(ns advent-of-code.day4
  (:require [clojure.string :as str]
            [advent-of-code.common :as common]
            [clojure.set :as set]))

(def puzzle-input (common/file->string-list "day4.txt"))

(defn get-points
  [n]
  (apply * (repeat (dec n) 2)))

(defn number-of-matches
  [card-numbers]
  (->> (map #(set/intersection (first %) (second %)) card-numbers)
       (map count)))

(defn parse-card-numbers
  [card-numbers]
  (map (fn [card] [(into #{} (common/numbers-in-string->number-seq (first card)))
                  (into #{} (common/numbers-in-string->number-seq (second card)))])
       card-numbers))

(defn parse-cards
  [cards]
  (->> (map #(str/replace % #"Card\s+\d+:" "") cards)
       (map #(str/split % #"\|"))))

(defn update-nr-of-cards
  [cards nr start end]
  (map-indexed (fn [idx n] (if (<= start idx end)
                            (+ nr n)
                            n)) cards))

(def first-star (->> (parse-cards puzzle-input)
                     parse-card-numbers
                     number-of-matches
                     (filter #(not= % 0))
                     (map get-points)
                     (apply +)))

(def second-star
  (loop [number-matches (->> (parse-cards puzzle-input)
                             parse-card-numbers
                             number-of-matches)
         idx            0
         card-total     (repeat (count number-matches) 1)]
    (if (= idx (count number-matches))
      (apply + card-total)
      (recur number-matches
             (inc idx)
             (update-nr-of-cards card-total
                                 (nth card-total idx)
                                 (inc idx)
                                 (dec (+ (inc idx) (nth number-matches idx))))))))

