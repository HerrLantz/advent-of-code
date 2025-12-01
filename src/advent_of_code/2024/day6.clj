(ns advent-of-code.2024.day6
  (:require [advent-of-code.common :as common]
            [clojure.string :as str]))

(def test
  "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
")

(defn rotate
  [m]
  (->> (common/transpose m)
       (map reverse)))

(defn contains-guard?
  [l]
  (some #{\^} l))

(defn walk
  [l]
  (let [start (take-while #(not= \^ %) l)
        r     (drop-while #(not= \^ %) l)
        mid   (take-while #(not= \# %) r)
        rest  (drop-while #(not= \# %) r)]
    [(concat start
             (repeat (dec (count mid)) \X)
             '(\^)
             (if (empty? rest)))
     (empty? rest)]))

(defn init-lab
  [m]
  (->> (map #(into '() %) m)
       rotate))

(defn draw-path
  [m]
  (loop [m m]
    (let [new-m (map #(if (contains-guard? %) (first (walk %)) %) m)
          done? (some #(and (contains-guard? %) (second (walk %))) m)]
      (println new-m)
      (if done?
        new-m
        (recur (rotate new-m))))))

(->> (common/file->string-list "./2024/day6.txt")
     init-lab
     draw-path
     flatten
     (filter #(#{\X} %))
     count
     inc)

(some #(and (= % 2) :meow-meow) '(2))

