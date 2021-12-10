(ns advent-of-code.2021.day04
  (:require [clojure.string :as str]))

(def example
  "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7")

(def input (slurp "./resources/2021/day04.txt"))

(defn parse-board [board]
  (-> board
      (str/trim)
      (str/split #"\s+")
      (->> (mapv parse-long)
           (partition 5)
           (mapv vec))))

(defn parse [input]
  (let [[numbers boards] (str/split input #"\n" 2)]
    {:numbers (-> numbers (str/split #",")    (->> (mapv parse-long)))
     :boards  (-> boards  (str/split #"\n\n") (->> (mapv parse-board)))}))

(parse example)

(str/split example #"\n" 2)

(defn win-rows? [board nums]
  (let [num-set (set nums)]
    (when (some #(every? num-set %) board)
      (->> board
           (apply concat)
           (remove num-set)
           (reduce +)
           (* (last nums))))))

(defn transpose [board]
  (apply map vector board))

(defn winner? [board nums]
  (or (win-rows? board nums)
      (win-rows? (transpose board) nums)))

(defn part1
  ([] (part1 example))
  ([input]
   (let [{:keys [numbers boards]} (parse input)]
     (loop [n 1]
       (let [nums (take n numbers)]
         (or (some #(winner? % nums) boards)
             (recur (inc n))))))))

(defn part2
  ([] (part2 example))
  ([input]
   (let [{:keys [numbers boards]} (parse input)]
     (loop [n 1
            boards boards]
       (let [nums (take n numbers)
             boards+ (remove #(winner? % nums) boards)]
         (if (empty? boards+)
           (winner? (last boards) nums)
           (recur (inc n) boards+)))))))

(comment
  (part1)
  (part1 input)
  (part2)
  (part2 input))
