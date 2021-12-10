(ns advent-of-code.2021.day9
  (:require [clojure.string :as str]))

(def sample
  "2199943210
3987894921
9856789892
8767896789
9899965678")

(def input (slurp "./resources/2021/day9.txt"))

(defn parse [input]
  (->> input
       (str/split-lines)
       (map (fn [line] (map #(- (int %) (int \0)) line)))))

(parse sample)

(defn index [board i j]
  (nth (nth board i) j))

(defn lowest? [x coll]
  (->> coll
       (map (partial < x))
       (every? true?)))

(defn inside? [n m [i j]]
  (and (>= i 0) (< i n)
       (>= j 0) (< j m)))

(defn neighbors-pos [board i j]
  (let [n (-> board count)
        m (-> board first count)]
    (->> [[0 -1] [1 0] [0 1] [-1 0]]
         (map (fn [[x y]] [(+ i x) (+ j y)]))
         (filter (partial inside? n m)))))

(defn neighbors [board i j]
  (->> (neighbors-pos board i j)
       (map (fn [[x y]] (index board x y)))))

(defn lowest-pos [board]
  (for [i (range (count board))
        j (range (-> board first count))
        :let [item (index board i j)
              adjs (neighbors board i j)]
        :when (lowest? item adjs)]
    [i j]))

(defn lowest [board]
  (->> (lowest-pos board)
       (map (fn [[x y]] (index board x y)))))

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
          adjs  (->> (neighbors-pos board x y)
                     (filter (comp not visited))
                     (filter (fn [[x' y']] (not= 9 (index board x' y')))))]
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
