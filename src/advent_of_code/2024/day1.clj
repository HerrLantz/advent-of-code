(ns advent-of-code.2024.day1
  (:require [advent-of-code.common :as common]))

(def part1
  (->> (common/file->numbers "./2024/day1.txt")
       (partition 2)
       common/transpose
       (map sort)
       (apply mapv (fn [a b] (abs (- a b))))
       (apply +)))

(def part2
  (let [[nums freq] (->> (common/file->numbers "./2024/day1.txt")
                         (partition 2)
                         common/transpose
                         ((juxt first #(frequencies (second %)))))]
    (apply + (map #(* % (or (get freq %) 0)) nums))))

