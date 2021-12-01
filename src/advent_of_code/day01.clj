(ns advent-of-code.day01
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-input
  [input-file-name]
  (->> (io/resource input-file-name)
       slurp
       str/split-lines
       (map (fn [s] (Integer. s)))))

(defn count-increments
  [data-points]
  (->> (filter (fn [p] (> (second p) (first p))) data-points)
       count))

(def solve-part-one
  (->> (parse-input "day01.txt")
       (partition 2 1)
       count-increments))

(def solve-part-two
  (->> (parse-input "day01.txt")
       (partition 3 1)
       (map (fn [p] (apply + p)))
       (partition 2 1)
       count-increments))
