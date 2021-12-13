(ns advent-of-code.2021.day13
  (:require [clojure.string :as str]
            [clojure.java.math :as math]))

(def sample
  "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5")

(defn parse [input]
  (let [[dots intructions] (str/split input #"\n\n")]
    [(->> (str/split-lines dots)
          (map #(str/split % #","))
          (map (fn [[x y]] [(parse-long x) (parse-long y)])))
     (->> (str/split-lines intructions)
          (map #(str/split % #" "))
          (map last)
          (map #(str/split % #"\="))
          (map (fn [[c v]] [(keyword c) (parse-long v)])))]))

(def input (slurp "./resources/2021/day13.txt"))

(defmulti fold (fn [_ intructions] (first intructions)))

(defmethod fold :x
  [dots [_ x]]
  (set (map (fn [[x1 y1]] [(- x (math/abs (- x x1))) y1]) dots)))

(defmethod fold :y
  [dots [_ y]]
  (set (map (fn [[x1 y1]] [x1 (- y (math/abs (- y y1)))]) dots)))

(defn part1
  ([] (part1 sample))
  ([input]
   (let [[dots intructions] (parse input)]
     (count (fold dots (first intructions))))))

(defn part2
  ([] (part2 sample))
  ([input]
   (let [[dots instructions] (parse input)]
     (loop [dots dots
            instructions instructions]
       (if (empty? instructions)
         dots
         (recur (fold dots (first instructions)) (rest instructions)))))))

(defn print-paper [dots]
  (let [x-max (->> dots (map first) (apply max) inc)
        y-max (->> dots (map second) (apply max) inc)]
    (print (apply str (repeat x-max "-")))
    (doseq [y (range y-max)
            x (range x-max)]
      (when (= 0 x)
        (print "\n"))
      (print (if (dots [x y]) \# \.)))
    (println "")
    (println (apply str (repeat x-max "-")))))

(defn main []
  (println "sample 1: " (part1))
  (println "input 1: " (part1 input)))

(comment
  (print-paper (part2 input)))
