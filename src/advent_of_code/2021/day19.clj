(ns advent-of-code.2021.day19
  (:require [clojure.string :as str]
            [fastmath.vector :as vector]
            [advent-of-code.common.core :refer [sign]]
            [clojure.set :as set]
            [clojure.java.math :as math]))

(def sample (slurp "./resources/2021/day19-sample.txt"))
(def input  (slurp "./resources/2021/day19.txt"))

(defn parse [input]
  (vec (for [sc (str/split input #"\n\n")
             :let [nums (->> sc (re-seq #"-?\d+") (map parse-long))]]
         (->> nums (drop 1) (partition 3) (mapv vector/seq->vec3) set))))

(def rotations
  (for [p [[1 2 3]
           [-2 1 3]
           [-1 -2 3]
           [2 -1 3]
           [-1 2 -3]
           [2 1 -3]
           [1 -2 -3]
           [-2 -1 -3]
           [-3 2 1]
           [-3 1 -2]
           [-3 -2 -1]
           [-3 -1 2]
           [3 2 -1]
           [3 1 2]
           [3 -2 1]
           [3 -1 -2]
           [1 -3 2]
           [-2 -3 1]
           [-1 -3 -2]
           [2 -3 -1]
           [1 3 -2]
           [-2 3 -1]
           [-1 3 2]
           [2 3 1]]]
    [(vector/seq->vec3 (map (comp dec math/abs) p))
     (vector/seq->vec3 (map sign p))]))

(defn rotate-one [point [idx dxyz]]
  (-> point
      (vector/permute idx)
      (vector/emult dxyz)))

(defn rotate-all [points rotation]
  (for [point points]
    (rotate-one point rotation)))

(defn solve [[beacons & scans]]
  (loop [global beacons
         [beacons & scans] scans
         scanners [(vector/vec3 0 0 0)]]
    (if (nil? beacons)
      [scanners global]
      (let [[[scanner match]]
            (for [rotation rotations
                  :let [beacons (rotate-all beacons rotation)]
                  g global
                  b beacons
                  :let [scanner (vector/sub g b)
                        beacons (set (map #(vector/add % scanner) beacons))]
                  :when (<= 12 (count (set/intersection global beacons)))]
              [scanner beacons])]
        (if scanner
          (recur (into global match) scans (conj scanners scanner))
          (recur global (concat scans [beacons]) scanners))))))

(defn part1
  ([] (part1 sample))
  ([input]
   (let [[_scanners beacons] (solve (parse input))]
     (count beacons))))

(defn part2
  ([] (part2 sample))
  ([input]
   (let [[scanners _beacons] (solve (parse input))]
     (apply max (for [sc1 scanners
                      sc2 scanners]
                  (vector/dist-abs sc1 sc2))))))

(comment
  (part1 input)
  (part2 input))
