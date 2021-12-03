(ns advent-of-code.day03
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn list-of-strings->list-of-numbers
  [l]
  (map (fn [s] (Integer. s)) l))

(defn list-of-bits->number
  [l]
  (-> (apply str l)
      (Integer/parseInt 2)))

(defn transpose
  [m]
  (apply mapv vector m))

(defn parse-input
  [input-file-name]
  (->> (io/resource input-file-name)
       slurp
       str/split-lines
       (map (fn [line] (str/split line #"")))
       (map list-of-strings->list-of-numbers)))

(defn calc-gamma
  [input]
  (as-> (transpose input) $
    (map (fn [l] (reduce + l)) $)
    (map (fn [n] (if (>= n (/ (count input) 2))
                   "1"
                   "0")) $)
    (apply str $)
    (Integer/parseInt $ 2)))

(defn calc-epsilon
  [input]
  (-> (calc-gamma input)
      (bit-xor 2r111111111111)))

(defn filter-by-bit-at-pos
  [l bit pos]
  (filter (fn [b] (bit-test (get b pos) bit)) l))

(defn calc-rating
  [input compare-fn]
  (loop [l   input
         idx 0]
    (let [ones   (filter (fn [l] (= (nth l idx) 1)) l)
          zeroes (filter (fn [l] (= (nth l idx) 0)) l)
          new-l  (if (compare-fn (count ones) (count zeroes))
                   ones
                   zeroes)]
      (if (= (count new-l) 1)
        (list-of-bits->number (first new-l))
        (recur new-l (inc idx))))))

(def solve-part-one
  (let [input   (parse-input "day03.txt")
        gamma   (calc-gamma input)
        epsilon (calc-epsilon input)]
    (* epsilon gamma)))

(def solve-part-two
  (let [input         (parse-input "day03.txt")
        gamma         (calc-gamma input)
        epsilon       (calc-epsilon input)
        co2-rating    (calc-rating input <)
        oxygen-rating (calc-rating input >=)]
    (* oxygen-rating co2-rating)))
