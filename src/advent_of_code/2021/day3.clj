(ns advent-of-code.2021.day3
  (:require [clojure.string :as str]
            [clojure.java.math :as math]))

(def example
  "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(def input (slurp "./resources/2021/input3.txt"))

(defn part1
  ([] (part1 input))
  ([input]
   (let [gamma   (->> (str/split-lines input)
                      (apply map (fn [& xs] (map #(-> % {\0 -1 \1 1}) xs)))
                      (map #(reduce + 0 %))
                      (map #(/ % (math/abs %)))
                      (map #(/ (+ % 1) 2)))
         epsilon (map #(- 1 %) gamma)]
     (->> [gamma epsilon]
          (map #(apply str %))
          (map #(Integer/parseInt % 2))
          (reduce * 1)))))

(defn ones? [lines index]
  (let [{zeros \0 ones \1} (->> lines
                                (map #(nth % index))
                                frequencies)]
    (>= ones zeros)))

(defn rating [lines ones zeros]
  (loop [lines lines
         index 0]
    (if (<= (count lines) 1)
      (Integer/parseInt (first lines) 2)
      (let [winner (if (ones? lines index) ones zeros)
            lines  (filter #(= winner (nth % index)) lines)]
        (recur lines (inc index))))))

(defn part2
  ([] (part2 input))
  ([input]
   (let [lines (str/split-lines input)
         O2    (rating lines \1 \0)
         CO2   (rating lines \0 \1)]
     O2)))

(comment
  (part1 example)
  (part1)
  (part2 example))
