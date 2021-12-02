(ns advent-of-code.day02
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-input
  [input-file-name]
  (->> (io/resource input-file-name)
       slurp
       str/split-lines
       (map (fn [s] (as-> (str/split s #"\s") $
                      [(first $) (Integer. (second $))])))))

(defn calc-position
  [data-points]
  (reduce (fn [acc curr] (condp = (first curr)
                           "forward" (update acc :horizon + (second curr))
                           "down"    (update acc :depth + (second curr))
                           "up"      (update acc :depth - (second curr))))
          {:depth 0 :horizon 0}
          data-points))

(defn calc-position2
  [data-points]
  (reduce (fn [acc curr] (condp = (first curr)
                           "forward" (-> (update acc :horizon + (second curr))
                                         (update :depth + (* (second curr) (:aim acc))))
                           "down"    (update acc :aim + (second curr))
                           "up"      (update acc :aim - (second curr))))
          {:depth 0 :horizon 0 :aim 0}
          data-points))

(def solve-part-one
  (as-> (parse-input "day02.txt") $
    (calc-position $)
    (* (:depth $) (:horizon $))))

(def solve-part-two
  (as-> (parse-input "day02.txt") $
    (calc-position2 $)
    (* (:depth $) (:horizon $))))

