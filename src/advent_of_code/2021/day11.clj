(ns advent-of-code.2021.day11
  (:require [clojure.string :as str]))

(def sample
  "5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526")

(def input (slurp "./resources/2021/day11.txt"))

(defn parse [input]
  (->> (str/split-lines input)
       (mapv #(mapv (comp parse-long str) %))))

(defn inside? [n m [i j]]
  (and (>= i 0) (< i n)
       (>= j 0) (< j m)))

(defn neighbors-pos [grid [i j]]
  (let [n (-> grid count)
        m (-> grid first count)]
    (->> [[0 -1] [1 0] [0 1] [-1 0] [-1 -1] [1 -1] [1 1] [-1 1]]
         (map (fn [[x y]] [(+ i x) (+ j y)]))
         (filter (partial inside? n m)))))

(defn index-at [grid i j]
  (nth (nth grid i) j))

(defn calc-flashed [grid flashed]
  (set (for [i (range (-> grid count))
             j (range (-> grid first count))
             :when  (and (> (index-at grid i j) 9)
                         (nil? (flashed [i j])))]
         [i j])))

(defn inc-grid [grid flashed old-flashed]
  (let [adjs (->> flashed
                  (mapcat #(neighbors-pos grid %))
                  (remove old-flashed)
                  frequencies)]
    (map-indexed
     (fn [idx x]
       (map-indexed
        (fn [idy y]
          (cond
            (flashed [idx idy]) 0
            (adjs [idx idy]) (+ y (adjs [idx idy]))
            :else y))
        x))
     grid)))

(defn next-step* [grid visited]
  (loop [grid    grid
         flashed visited]
    (let [new-flashed (calc-flashed grid flashed)]
      (if (empty? new-flashed)
        [grid (count flashed)]
        (recur (inc-grid grid new-flashed flashed) (apply conj flashed new-flashed))))))

(defn next-step [[grid _]]
  (next-step* (map #(map inc %) grid) #{}))

(defn part1
  ([] (part1 sample))
  ([input]
   (->> [(parse input) 0]
        next-step
        (iterate next-step)
        (take 100)
        (map second)
        (reduce +))))

(defn part2
  ([] (part2 sample))
  ([input]
   (->> [(parse input) 0]
        next-step
        (iterate next-step)
        (take-while (fn [[_ ans]] (not= 100 ans)))
        count
        inc)))

(defn main []
  (println "sample 1: " (part1))
  (println "input 1: " (part1 input))
  (println "sample 2: " (part2))
  (println "input 2: " (part2 input)))

(comment
  (main))
