(ns advent-of-code.day2
  (:require [advent-of-code.common :as common]))

(defn get-max-color
  [color s]
  (let [pattern (str "(\\d+) " color)
        color-amount (map #(Integer. (second %)) (re-seq (re-pattern pattern) s))]
    (if (empty? color-amount)
      0
      (apply max color-amount))))

(defn get-max-colors
  [game]
  [(get-max-color "red" game)
   (get-max-color "green" game)
   (get-max-color "blue" game)])

(defn valid-game?
  [game]
  (let [[r g b] (get-max-colors game)]
    (and (<= r 12)
         (<= g 13)
         (<= b 14))))

(defn cube-power
  [game]
  (apply * (get-max-colors game)))

(defn game-id
  [game]
  (->> (re-find #"Game (\d+)" game)
       second
       Integer.))

(def first-star (->> (common/file->string-list "day2.txt")
                     (filter valid-game?)
                     (map game-id)
                     (apply +)))

(def second-star (->> (common/file->string-list "day2.txt")
                      (map cube-power)
                      (apply +)))
