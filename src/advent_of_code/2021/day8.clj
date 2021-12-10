(ns advent-of-code.2021.day8
  (:require [clojure.string :as str]))

(def sample (slurp  "./resources/2021/day8-sample.txt"))

(def input (slurp "./resources/2021/day8.txt"))

(defn sort-str [s]
  (apply str (sort s)))

(defn parse [input]
  (->> input
       str/split-lines
       (map #(str/split % #" \| "))
       (map (fn [[s o]] [(str/split s #" ") (str/split o #" ")]))
       (map (fn [[s o]] [(mapv sort-str s) (mapv sort-str o)]))
       (map (fn [[s o]] [(group-by count s) o]))
       (map (fn [[s o]] [(filter (fn [[_ v]] (= 1 (count v))) s) o]))
       (map (fn [[s o]] [(mapcat identity (vals s)) o]))
       (map (fn [[s o]] [(set s) o]))))

(parse sample)

(defn part1
  ([] (part1 sample))
  ([input]
   (->> (parse input)
        (map (fn [[s o]] (reduce + (map #(if (s %) 1 0) o))))
        (reduce +))))

;;  0000    0000    ....    2222   3333    ....     5555    6666    7777    8888    9999
;; 1    2  0    0  .    1  .    2  .    3  4    4  5    .  6    .  .    7  8    8  9    9
;; 1    2  0    0  .    1  .    2  .    3  4    4  5    .  6    .  .    7  8    8  9    9
;;  3333   ....    ....    2222    3333     4444    5555    6666    ....    8888    9999
;; 4    5  0    0  .    1  2    .  .    3  .    4  .    5  6    6  .    7  8    8  .    9
;; 4    5  0    0  .    1  2    .  .    3  .    4  .    5  6    6  .    7  8    8  .    9
;;  6666    0000    ....    2222   3333    ....    5555    6666    ....     8888    9999

(def numbers
  {[0 1 2 4 5 6]   0
   [2 5]           1
   [0 2 3 4 6]     2
   [0 2 3 5 6]     3
   [1 2 3 5]       4
   [0 1 3 5 6]     5
   [0 1 3 4 5 6]   6
   [0 2 5]         7
   [0 1 2 3 4 5 6] 8
   [0 1 2 3 5 6]   9})

(def permutations
  (for [a (range 0 7)
        b (range 0 7)
        :when (not (#{a} b))
        c (range 0 7)
        :when (not (#{a b} c))
        d (range 0 7)
        :when (not (#{a b c} d))
        e (range 0 7)
        :when (not (#{a b c d} e))
        f (range 0 7)
        :when (not (#{a b c d e} f))
        g (range 0 7)
        :when (not (#{a b c d e f} g))
        :let [permutation [a b c d e f g]]]
    permutation))

(defn parse2 [input]
  (->> input
       str/split-lines
       (map #(str/split % #" \| "))
       (map (fn [[s o]] [(str/split s #" ") (str/split o #" ")]))))

(defn decode [segment code]
  (->> segment
       (map #(- (int %) (int \a)))
       (map #(nth code %))
       sort))

(defn solution? [code signal]
  (let [decodes (mapv #(decode % code) signal)]
    (when (every? numbers decodes)
      code)))

(defn solve [[signal output]]
  (let [code (->> permutations
                  (keep #(solution? % signal))
                  first)]
    (map #(numbers (decode % code)) output)))

(defn part2
  ([] (part2 sample))
  ([input]
   (->> (parse2 input)
        (map solve)
        (map #(apply str %))
        (map parse-long)
        (reduce +))))

(defn main []
  (println "sample 1: " (part1))
  (println "input 1: " (part1 input))
  (println "sample 2: " (part2))
  (println "input 2: " (part2 input)))

(comment
  (main))
