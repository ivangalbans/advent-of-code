(ns advent-of-code.2021.day05
  (:require [clojure.string :as str]
            [clojure.java.math :as math]))

(def sample
  "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(def input (slurp "./resources/2021/day05.txt"))

(defn parse-point [[x y]]
  [(parse-long x) (parse-long y)])

(defn parse [input]
  (->> input
       str/split-lines
       (map #(str/split % #" -> "))
       (map (fn [[p1 p2]] [(str/split p1 #",") (str/split p2 #",")]))
       (map (fn [[p1 p2]] [(parse-point p1) (parse-point p2)]))))

(defn hv-line? [[[x1 y1] [x2 y2]]]
  (or (= x1 x2)
      (= y1 y2)))

(defn diagonal? [[[x1 y1] [x2 y2]]]
  (= (math/abs (- x1 x2)) (math/abs (- y1 y2))))

(defn sign [x]
  (if (zero? x)
    0
    (/ x (math/abs x))))

(defn expand-line [[[x1 y1] [x2 y2]]]
  (if (diagonal? [[x1 y1] [x2 y2]])
    (let [dx (sign (- x2 x1))
          dy (sign (- y2 y1))
          x  (range x1 (+ dx x2) dx)
          y  (range y1 (+ dy y2) dy)]
      (map #(vector %1 %2) x y))
    (for [x (range (min x1 x2) (inc (max x1 x2)))
          y (range (min y1 y2) (inc (max y1 y2)))]
      [x y])))

(defn solve [input valid-line?]
  (->> (parse input)
       (filter valid-line?)
       (mapcat expand-line)
       frequencies
       vals
       (remove #(= 1 %))
       count))

(defn part1
  ([]      (part1 sample))
  ([input] (solve input hv-line?)))

(defn part2
  ([]      (part2 sample))
  ([input] (solve input #(or (hv-line? %) (diagonal? %)))))

(comment
  (part1)
  (part1 input)
  (part2)
  (part2 input))
