(ns advent-of-code.2025.day1
  (:require [advent-of-code.common :as common]
            [clojure.string :as str]))

(defn parse-row
  [r]
  (let [dir (first r)
        n   (Integer. (str/join (rest r)))]
    (if (= dir \R)
      n
      (mod (- 100 n) 100))))

(defn parse-row-2
  [r]
  [(if (= (first r) \R) + -)
   (Integer. (str/join (rest r)))])

(defn next-dial-position
  [acc curr]
  (conj acc (mod (+ curr (first acc)) 100)))

(defn next-dial-position-2
  [acc [op n]]
  (let [prev (first (first acc))
        res  (op prev n)]
    (conj acc
          [(mod res 100)
           (->> (range (op prev 1) (op res 1) (op 1))
                (map #(mod %1 100))
                (filter zero?)
                count)])))

(def first-star (->> (common/file->string-list "./2025/day1.txt")
                     (map parse-row)
                     (reduce next-dial-position '(50))
                     (filter zero?)
                     count))

(def second-star (->> (common/file->string-list "./2025/day1.txt")
                      (map parse-row-2)
                      (reduce next-dial-position-2 '([50 0]))
                      reverse
                      (map second)
                      (apply +)))
