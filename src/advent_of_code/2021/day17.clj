(ns advent-of-code.2021.day17
  (:require [advent-of-code.common.core :refer [read-input]]
            [clojure.string :as str]))

(def sample "target area: x=20..30, y=-10..-5")

(def input (read-input 2021 17))

(defn parse [input]
  (let [[_ x1 x2 y1 y2] (re-matches
                         #"target area: x=(\-?[0-9]+)..(\-?[0-9]+), y=(\-?[0-9]+)..(\-?[0-9]+)"
                         (str/trim-newline input))]
    {:x1 (parse-long x1)
     :x2 (parse-long x2)
     :y1 (parse-long y1)
     :y2 (parse-long y2)}))

(def all-comb
  (for [dx (range 0 150)
        dy (range -160 160)]
    {:x 0 :y 0 :dx dx :dy dy}))

(defn next-jump [{:keys [x y dx dy]}]
  {:x  (+ x dx)
   :y  (+ y dy)
   :dx (cond (> dx 0) (dec dx)
             (< dx 0) (inc dx)
             :else 0)
   :dy (dec dy)})

(defn inside? [{:keys [x y]} {:keys [x1 x2 y1 y2]}]
  (and (<= x1 x x2)
       (<= y1 y y2)))

(defn after? [{:keys [x y]} {:keys [_ x2 y1 _]}]
  (or (> x x2)
      (< y y1)))

(defn simulate [path dst]
  (let [src (peek path)]
    (cond
      (inside? src dst) path
      (after?  src dst) nil
      :else (recur (conj path (next-jump src)) dst))))

(defn solve [dst]
  (keep #(simulate [%] dst) all-comb))

(defn part1
  ([] (part1 sample))
  ([input]
   (->> (solve (parse input))
        (map #(apply max-key :y %))
        (apply max-key :y)
        :y)))

(defn part2
  ([]      (part2 sample))
  ([input] (count (solve (parse input)))))

(comment
  (part1 input)
  (part2 input))
