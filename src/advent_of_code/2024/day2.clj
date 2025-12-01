(ns advent-of-code.2024.day2
  (:require [advent-of-code.common :as common]))

(defn safe?
  [l]
  (and (or (apply < l)
           (apply > l))
       (every? (fn [[a b]] (<= 1 (abs (- a b)) 3))
               (partition 2 1 l))))

(def part1 (->> (common/file->string-list "./2024/day2.txt")
                (map common/numbers-in-string->number-seq)
                (filter safe?)
                count))

(def part2 (->> (common/file->string-list "./2024/day2.txt")
                (map common/numbers-in-string->number-seq)
                (map common/list->remove-one-permutations)
                (filter #(some safe? %))
                count))

