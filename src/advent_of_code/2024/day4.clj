(ns advent-of-code.2024.day4
  (:require [advent-of-code.common :as common]))

(def part1
  (let [horizontal (common/file->string-list "./2024/day4.txt")
        vertical   (map #(apply str %) (common/transpose horizontal))
        left-diag  (map #(apply str %) (common/rotate-matrix-left horizontal))
        right-diag (map #(apply str %) (common/rotate-matrix-right horizontal))
        count-xmas #(count (re-seq #"XMAS" %))
        count-samx #(count (re-seq #"SAMX" %))]
    (+ (apply + (map count-xmas horizontal))
       (apply + (map count-samx horizontal))
       (apply + (map count-xmas vertical))
       (apply + (map count-samx vertical))
       (apply + (map count-xmas left-diag))
       (apply + (map count-samx left-diag))
       (apply + (map count-xmas right-diag))
       (apply + (map count-samx right-diag)))))

(defn x-mas?
  [[top bot]]
  (and (or (= top [\M \S])
           (= top [\S \M]))
       (or (= bot [\M \S])
           (= bot [\S \M]))))

(def part2
  (let [input   (map seq (common/file->string-list "./2024/day4.txt"))
        x-count (count (first input))
        y-count (count input)]
    (loop [curr-x 1
           curr-y 1
           total  0]
      (cond
        (= (inc curr-y) y-count)               total
        (= (inc curr-x) x-count)               (recur 1 (inc curr-y) total)
        (= (nth (nth input curr-y) curr-x) \A) (recur (inc curr-x)
                                                      curr-y
                                                      (if (x-mas? (common/get-diagonal-neighbours curr-x curr-y input))
                                                        (inc total)
                                                        total))
        :else                                  (recur (inc curr-x) curr-y total)))))
