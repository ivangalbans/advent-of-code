(ns advent-of-code.2021.day12
  (:require [clojure.string :as str]))

(def sample1
  "start-A
start-b
A-c
A-b
b-d
A-end
b-end")

(def sample2
  "dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc")

(def input (slurp "./resources/2021/day12.txt"))

(defn parse [input]
  (->> (str/split-lines input)
       (map #(str/split % #"\-"))
       (reduce (fn [acc [src dst]]
                 (-> acc
                     (update src (fnil conj []) dst)
                     (update dst (fnil conj []) src)))
               {})))

(defn lower-case? [s]
  (= s (str/lower-case s)))

(defn dfs [graph start end visited]
  (cond
    (= start end) 1
    (zero? (visited start)) 0
    :else (reduce
           + 0
           (map (fn [node]
                  (dfs graph node end (update visited start dec)))
                (get graph start)))))

(def oo 99999999)

(defn ends? [node]
  (#{"start" "end"} node))

(defn part1
  ([] (part1 sample1))
  ([input]
   (let [graph   (parse input)
         visited (->> (keys graph)
                      (map (fn [k] [k (if (lower-case? k) 1 oo)]))
                      (map (fn [[k v]] (hash-map k v)))
                      (apply merge))]
     (dfs graph "start" "end" visited))))

(defn part2
  ([] (part2 sample1))
  ([input]
   (let [graph    (parse input)
         visited  (->> (keys graph)
                       (map (fn [k] [k (if (lower-case? k) 1 oo)]))
                       (map (fn [[k v]] (hash-map k v)))
                       (apply merge))
         smalls-v (->> (keys visited)
                       (filter #(and (lower-case? %) (not (ends? %))))
                       (map #(update visited % inc)))]
     (- (reduce + (map #(dfs graph "start" "end" %) smalls-v))
        (* (part1 input) (-> smalls-v keys count dec))))))

(defn main []
  (println "sample 1: " (part1))
  (println "input 1: " (part1 input))
  (println "sample 2: " (part2))
  (println "input 2: " (part2 input)))

(comment
  (main))
