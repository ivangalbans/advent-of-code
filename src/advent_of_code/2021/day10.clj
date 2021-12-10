(ns advent-of-code.2021.day10
  (:require [clojure.string :as str]))

(def sample
  "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]")

(def input (slurp "./resources/2021/day10.txt"))

(defn parse [input]
  (str/split-lines input))

(def points {\) 3 \] 57 \} 1197 \> 25137})

(defn couple? [open closed]
  (= closed ({\( \) \[ \] \{ \} \< \>} open)))

(defn closed? [c]
  (points c))

(defn solve [cost-fn point-fn s]
  (loop [stack []
         index 0]
    (cond
      (= index (count s)) (cost-fn stack)
      (closed? (nth s index)) (if-not (couple? (peek stack) (nth s index))
                                (point-fn (nth s index))
                                (recur (pop stack) (inc index)))
      :else (recur (conj stack (nth s index)) (inc index)))))

(defn part1
  ([] (part1 sample))
  ([input]
   (->> (parse input)
        (map (partial solve (constantly 0) points))
        (reduce +))))

(defn cost-fn2 [stack]
  (let [points {\( 1 \[ 2 \{ 3 \< 4}]
    (reduce (fn [acc x]
              (+ (* acc 5) (points x)))
            0
            (reverse stack))))

(defn middle [coll]
  (nth (sort coll) (/ (count coll) 2)))

(defn part2
  ([] (part2 sample))
  ([input]
   (->> (parse input)
        (map (partial solve cost-fn2 (constantly 0)))
        (remove zero?)
        middle)))

(defn main []
  (println "sample 1: " (part1))
  (println "input 1: " (part1 input))
  (println "sample 2: " (part2))
  (println "input 2: " (part2 input)))

(comment
  (main))
