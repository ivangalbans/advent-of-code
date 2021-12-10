(ns advent-of-code.2021.day07
  (:require
   [clojure.string :as str]
   [clojure.java.math :as math]))

(def sample "16,1,2,0,4,2,7,1,2,14")

(def input (slurp "./resources/2021/day07.txt"))

(defn parse [input]
  (as-> input $
    (str/trim-newline $)
    (str/split $ #",")
    (map parse-long $)))

(defn fuel [cost-fn xs x]
  (->> (repeat x)
       (map cost-fn xs)
       (reduce +)))

;; O((max-min)*n)
(defn solve [input cost-fn]
  (let [xs (parse input)
        mi (reduce min xs)
        ma (reduce max xs)]
    (->> (range mi (inc ma))
         (map (partial fuel cost-fn xs))
         (reduce min))))

(defn sum [n]
  (/ (* n (+ n 1)) 2))

(defn part1
  ([]      (part1 sample))
  ([input] (solve input (comp math/abs -))))

(defn part2
  ([]      (part2 sample))
  ([input] (solve input (comp sum math/abs -))))

(comment
  (part1)
  (part1 input)
  (part2)
  (part2 input))
