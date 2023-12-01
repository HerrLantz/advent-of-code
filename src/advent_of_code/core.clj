(ns advent-of-code.core
  (:require [advent-of-code.common :as common]))

(def first-star
  (->> (common/file->string-list "day1.txt")
       (map (fn [s] (re-seq #"\d+" s)))
       (map (fn [nums] (apply str nums)))
       (map (fn [str-num] (Integer. (str (first str-num) (last str-num)))))
       (apply +)))

(def second-star
  (->> (common/file->string-list "day1.txt")
       (map common/split-number-words-and-number-character)
       (map #(map common/number-word->number-character %))
       (map (fn [n] (Integer. (str (first n) (last n)))))
       (apply +)))

