(ns advent-of-code.common
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn file->string-list
  [input-file-name]
  (->> (io/resource input-file-name)
       slurp
       str/split-lines))

(defn file->string
  [input-file-name]
  (->> (io/resource input-file-name)
       slurp))

(defn file->numbers
  [input-file-name]
  (->> (io/resource input-file-name)
       slurp
       (re-seq #"\d+")
       (map #(Integer/parseInt %))))

(defn split-empty-lines
  [s]
  (str/split s #"\n{2,}"))

(defn string-list->number-list
  [l]
  (map read-string l))

(defn number-list->string-list
  [s & [separator]]
  (let [value-separator (or separator " ")]
    (->> (repeat value-separator)
         (interleave s)
         drop-last
         (apply str))))

(defn list-of-bits->number
  [l]
  (-> (apply str l)
      (Integer/parseInt 2)))

(defn transpose
  [m]
  (apply mapv vector m))

(defn upper-case?
  [s]
  (let [s2 (str s)]
    (-> (str/upper-case s2)
        (= s2))))

(defn lower-case?
  [s]
  (let [s2 (str s)]
    (-> (str/lower-case s2)
        (= s2))))

(defn numbers-in-string->number-seq
  [s]
  (->> (re-seq #"\d+" s)
       (map (fn [n] (Integer. n)))))

(defn sum-numbers-in-str
  [s]
  (->> numbers-in-string->number-seq s
       (apply +)))

(defn hex?
  [s]
  (re-matches #"[0-9a-fA-F]+" s))

(defn sum-hex->number
  [h & h2]
  (->> (conj h2 h)
       (map (fn [hex] (Integer/parseInt hex 16)))
       (apply +)))

(defn sum-hex->hex
  [h & h2]
  (->> (apply sum-hex->number (conj h2 h))
       (format "%X")))


(defn string->binary-string-list
  [s]
  (->> (sequence s)
       (map int)
       (map (fn [n] (Integer/toString n 2)))
       (map (fn [n] (str (apply str (repeat (- 8 (count n)) "0"))
                        n)))))

(defn string->binary-string
  [s]
  (apply str (string->binary-string-list s)))

(defn binary-string->character-string
  [binary-string]
  (->> (re-seq #".{8}" binary-string)
       (map (fn [b] (char (Long/parseLong b 2))))
       (apply str)))

(defn split-str-in-middle
  [s]
  (let [middle (/ (count s) 2)]
    (->> (split-at middle s)
         (map set))))

(defn all-unique?
  [l]
  (= (count l) (count (set l))))

(defn number-word->number-character
  [s]
  (as-> s $
    (str/replace $ #"one"   "1")
    (str/replace $ #"two"   "2")
    (str/replace $ #"three" "3")
    (str/replace $ #"four"  "4")
    (str/replace $ #"five"  "5")
    (str/replace $ #"six"   "6")
    (str/replace $ #"seven" "7")
    (str/replace $ #"eight" "8")
    (str/replace $ #"nine"  "9")))

(defn split-number-words-and-number-character
  [s]
  (map second (re-seq #"(?<=(one|two|three|four|five|six|seven|eight|nine|\d+))" s)))

(defn occurences
  [word s overlapping?]
  (let [pattern (if overlapping?
                  (str "(?<=(" word "))")
                  word)]
    (count (re-seq (re-pattern pattern) s))))

(defn list->remove-one-permutations
  [l]
  (map-indexed #(keep-indexed (fn [idx n] (when (not= idx %1) n)) %2)
               (repeat (count l) l)))

(defn rotate-left
  [n s]
  (apply str (concat (drop n s)
                     (take n s))))

(defn rotate-right
  [n s]
  (apply str (concat (drop (- (count s) n) s)
                     (take (- (count s) n) s))))

(defn rotate-left-and-fill
  [n r s]
  (apply str (concat (drop n s)
                     (repeat n r))))

(defn rotate-right-and-fill
  [n r s]
  (apply str (concat (repeat n r)
                     (take (- (count s) n) s))))

(defn pad-left
  [n v l]
  (concat (repeat n v) l))

(defn pad-right
  [n v l]
  (concat l (repeat n v)))

(defn rotate-matrix-left
  [l]
  (->> (map-indexed (fn [idx v] (pad-right (- (count l) (inc idx)) nil (pad-left idx nil v))) l)
       (map reverse)
       transpose
       (map #(filter some? %))))

(defn rotate-matrix-right
  [l]
  (->> (map-indexed (fn [idx v] (pad-left (- (count l) (inc idx)) nil (pad-right idx nil v))) l)
       (map reverse)
       transpose
       (map #(filter some? %))))

(defn get-diagonal-neighbours
  [x y mat]
  [[(nth (nth mat (dec y)) (dec x)) (nth (nth mat (inc y)) (inc x))]
   [(nth (nth mat (inc y)) (dec x)) (nth (nth mat (dec y)) (inc x))]])

(defn drop-repeatedly
  [l]
  (map #(drop % l) (range (count l))))

(defn middle
  [l]
  (nth l (quot (count l) 2)))

(defn keyword->value
  [k]
  (read-string (name k)))

(defn str->substrs
  [s]
  (map #(str/join (take %1 s)) (range 1 (inc (count s)))))
