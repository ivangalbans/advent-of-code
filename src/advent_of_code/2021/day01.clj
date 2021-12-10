(ns advent-of-code.2021.day01
  (:require [clojure.string :as str]))

(def input (->> (slurp "./resources/2021/day01.txt")
                str/split-lines
                (map parse-long)))

(def ans1 (->> (map - (rest input) input)
               (filter pos?)
               count))
ans1 ; => 1374

;; part2

(def ans2 (as-> (map + (drop 2 input) (drop 1 input) input) $
            (map - (rest $) $)
            (filter pos? $)
            (count $)))
ans2 ; => 1418
