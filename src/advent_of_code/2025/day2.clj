(ns advent-of-code.2025.day2
  (:require [clojure.string :as str]
            [advent-of-code.common :as common]))

(def test-input "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124")

(defn repeats?
  [n]
  (= (str/join (take (int (/ (count n) 2)) n))
     (str/join (drop (int (/ (count n) 2)) n))))

(defn repeats-2?
  [n]
  (let [len  (count n)
        strs (->> (common/str->substrs n)
                  (take (/ len 2))
                  (map #(str/join (repeat (/ len (count %1)) %1))))]
    (some #(and (> (count %1) 1) (= n %1)) strs)))

(defn parse-input
  [input]
  (->> (str/split input  #",")
       (map str/trim)
       (map #(str/split %1 #"-"))
       (map (fn [[a b]] [(BigInteger. a) (inc (BigInteger. b))]))
       (map #(apply range %1))
       flatten
       (map str)))

(def star-one (->> (common/file->string "./2025/day2.txt")
                   parse-input
                   (filter repeats?)
                   (map #(BigInteger. %1))
                   (apply +)))

(def star-two (->> (common/file->string "./2025/day2.txt")
                   parse-input
                   (filter repeats-2?)
                   (map #(BigInteger. %1))
                   (apply +)))
