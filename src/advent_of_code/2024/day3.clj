(ns advent-of-code.2024.day3
  (:require [advent-of-code.common :as common]))

(def part1 (->> (common/file->string "./2024/day3.txt")
                (re-seq #"mul\(\d{1,3},\d{1,3}\)")
                (map common/numbers-in-string->number-seq)
                (map #(apply * %))
                (apply +)))

(defn remove-disabled
  [l]
  (loop [l     l
         res-l '()
         keep? true]
    (if (empty? l)
      res-l
      (cond
        (true? (first l))  (recur (rest l) res-l true)
        (false? (first l)) (recur (rest l) res-l false)
        :else              (recur (rest l)
                                  (if keep?
                                    (conj res-l (first l))
                                    res-l)
                                  keep?)))))

(def part2 (->> (common/file->string "./2024/day3.txt")
                (re-seq #"mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)")
                (map #(common/re-place-with-value #"do\(\)" true %))
                (map #(common/re-place-with-value #"don't\(\)" false %))
                remove-disabled
                (map common/numbers-in-string->number-seq)
                (map #(apply * %))
                (apply +)))

