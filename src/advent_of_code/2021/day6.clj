(ns advent-of-code.2021.day6
  (:require [clojure.string :as str]))

(def sample "3,4,3,1,2")

(def input (slurp "./resources/2021/day6.txt"))

(defn reproduce [fishes]
  (reduce-kv
   (fn [fishes' day nfishes]
     (if (zero? day)
       (-> fishes'
           (assoc 8 nfishes)
           (update 6 (fnil + 0) nfishes))
       (-> fishes'
           (update (dec day) (fnil + 0) nfishes))))
   {}
   fishes))

(defn solve [input days]
  (let [fishes (->> (str/split (str/trim-newline input) #",")
                    (map parse-long))]
    (loop [fishes' (frequencies fishes)
           day 0]
      (if (>= day days)
        (reduce + (vals fishes'))
        (recur (reproduce fishes') (inc day))))))

(defn part1
  ([]      (part1 sample))
  ([input] (solve input 80)))

(solve input 0)

(defn part2
  ([]      (part2 sample))
  ([input] (solve input 256)))

(comment
  (part1)
  (part1 input)
  (part2)
  (part2 input))
