(ns advent-of-code.2021.day09
  (:require [clojure.string :as str]
            [advent-of-code.common.core :refer [index-at neighbors4 hvneighbors4-pos))

(def sample
  "2199943210
3987894921
9856789892
8767896789
9899965678")

(def input (slurp "./resources/2021/day09.txt"))

(defn parse [input]
  (->> input
       (str/split-lines)
       (map (fn [line] (map #(- (int %) (int \0)) line)))))

(defn lowest? [x coll]
  (->> coll
       (map (partial < x))
       (every? true?)))

(defn lowest-pos [board]
  (for [i (range (count board))
        j (range (-> board first count))
        :let [item (index-at board i j)
              adjs (neighbors4 board i j)]
        :when (lowest? item adjs)]
    [i j]))

(defn lowest [board]
  (->> (lowest-pos board)
       (map #(index-at board %))))

(defn part1
  ([] (part1 sample))
  ([input]
   (->> (parse input)
        lowest
        (map inc)
        (reduce +))))

(defn basin [board initial]
  (loop [queue   [initial]
         visited #{}]
    (let [[x y] (peek queue)
          adjs  (->> (neighbors4-pos board x y)
                     (filter (comp not visited))
                     (filter #(not= 9 (index-at board %))))]
      (if (empty? queue)
        visited
        (recur (apply conj (pop queue) adjs) (conj visited [x y]))))))

(defn part2
  ([] (part2 sample))
  ([input]
   (let [board (parse input)]
     (->> (lowest-pos board)
          (map (partial basin board))
          (into #{})
          (map count)
          (sort #(compare %2 %1))
          (take 3)
          (reduce *)))))

(defn main []
  (println "sample 1: " (part1))
  (println "input 1: " (part1 input))
  (println "sample 2: " (part2))
  (println "input 2: " (part2 input)))
