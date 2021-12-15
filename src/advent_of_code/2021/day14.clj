(ns advent-of-code.2021.day14
  (:require [clojure.string :as str]
            [advent-of-code.common.core :refer [window read-input]]))

(def sample
  "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C")

(def input (read-input 2021 14))

(defn parse [input]
  (let [[polymer rules] (str/split input #"\n\n")]
    [polymer (->> (str/split-lines rules)
                  (map #(str/split % #" -> "))
                  (map (fn [[k v]] (hash-map k v)))
                  (reduce merge))]))

(def next-steps
  (memoize
   (fn [polymer rules n]
     (if (zero? n)
       (frequencies polymer)
       (->> (window polymer)
            (map (fn [[c1 c2]]
                   (next-steps (str c1 (rules (str c1 c2)) c2) rules (dec n))))
            (cons (-> polymer rest butlast frequencies (update-vals -)))
            (apply merge-with +))))))

(defn solve [input step]
  (let [[polymer rules] (parse input)]
    (->> (next-steps polymer rules step)
         vals
         (apply (juxt max min))
         (apply -))))

(defn part1
  ([]      (part1 sample))
  ([input] (solve input 10)))

(defn part2
  ([]      (part2 sample))
  ([input] (solve input 40)))

(defn main []
  (println "sample 1:" (part1))
  (println "input 1:" (part1 input))
  (println "sample 2:" (part2))
  (println "input 2:" (part2 input)))

(comment
  (main))
