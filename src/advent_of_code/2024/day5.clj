(ns advent-of-code.2024.day5
  (:require [advent-of-code.common :as common]
            [clojure.set :as set]))

(defn in-order?
  [n r ord]
  (if (nil? r)
    true
    (not (contains? (set (apply set/union
                                (filter some? ((apply juxt r) ord))))
                    (read-string (name n))))))

(defn get-score
  [l ord]
  (every? (fn [[f & r]] (in-order? f r ord)) (common/drop-repeatedly l)))

(defn parse-order
  [s]
  (->> (partition 2 (common/numbers-in-string->number-seq s))
       (reduce (fn [acc curr] (update acc
                                     (keyword (str (first curr)))
                                     #(conj (set %) (second curr))))
               {})))

(defn count-score
  [l]
  (reduce (fn [acc curr] (+ acc (read-string (name (common/middle curr)))))
          0
          l))

(defn parse-update
  [s]
  (->> (map #(common/numbers-in-string->number-seq %) (clojure.string/split-lines s))
       (map (fn [l] (map #(keyword (str %)) l)))))

(defn with-order
  [a b ord]
  (if (some #{(read-string (name b))} (get ord a))
    -1
    1))

(def part1
  (let [[ord upd] (common/split-empty-lines (common/file->string "./2024/day5.txt"))
        upd       (parse-update upd)
        ord       (parse-order ord)]
    (count-score (filter #(get-score % ord) upd))))

(def part2
  (let [[ord upd] (common/split-empty-lines (common/file->string "./2024/day5.txt"))
        upd       (parse-update upd)
        ord       (parse-order ord)
        valid     (filter #(get-score % ord) upd)
        invalid   (set/difference (set upd) (set valid))
        fixed     (map #(sort (fn [a b] (with-order a b ord)) %) invalid)]
    (->> fixed
         (map common/middle)
         (map common/keyword->value)
         (apply +))))
