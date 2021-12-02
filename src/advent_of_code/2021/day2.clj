(ns advent-of-code.2021.day2
  (:require [clojure.string :as str]))

(def input (->> (slurp "./resources/2021/input2.txt")
                str/split-lines
                (map #(str/split % #" "))))

(defn dvector [d v]
  (get {"forward" [v 0]
        "down"    [0 v]
        "up"      [0 (- v)]}
       d))

(def ans1 (->> input
               (map (fn [[d v]] (dvector d (parse-long v))))
               (reduce (fn [[x1 y1] [x2 y2]] [(+ x1 x2) (+ y1 y2)]))
               (apply *)))

ans1 ;; => 1694130

;; part 2

(defn dvector2 [d v]
  (get {"forward" [v 0 0]
        "down"    [0 0 v]
        "up"      [0 0 (- v)]}
       d))

(def ans2 (->> input
               (map (fn [[d v]] (dvector2 d (parse-long v))))
               (reduce (fn [[x1 y1 z1] [x2 _ z2]] [(+ x1 x2) (+ y1 (* x2 z1)) (+ z1 z2)]))
               (take 2)
               (apply *)))

ans2 ;; => 1698850445
