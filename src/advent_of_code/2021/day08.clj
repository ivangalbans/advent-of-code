(ns advent-of-code.2021.day08
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as comb]))

(def sample
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
")

(def input (slurp "./resources/2021/day08.txt"))

(defn sort-str [s]
  (apply str (sort s)))

(defn parse [input]
  (->> input
       str/split-lines
       (map #(str/split % #" \| "))
       (map (fn [[s o]] [(str/split s #" ") (str/split o #" ")]))
       (map (fn [[s o]] [(mapv sort-str s) (mapv sort-str o)]))
       (map (fn [[s o]] [(group-by count s) o]))
       (map (fn [[s o]] [(filter (fn [[_ v]] (= 1 (count v))) s) o]))
       (map (fn [[s o]] [(mapcat identity (vals s)) o]))
       (map (fn [[s o]] [(set s) o]))))

(parse sample)

(defn part1
  ([] (part1 sample))
  ([input]
   (->> (parse input)
        (map (fn [[s o]] (reduce + (map #(if (s %) 1 0) o))))
        (reduce +))))

;;  0000    0000    ....    2222   3333    ....     5555    6666    7777    8888    9999
;; 1    2  0    0  .    1  .    2  .    3  4    4  5    .  6    .  .    7  8    8  9    9
;; 1    2  0    0  .    1  .    2  .    3  4    4  5    .  6    .  .    7  8    8  9    9
;;  3333   ....    ....    2222    3333     4444    5555    6666    ....    8888    9999
;; 4    5  0    0  .    1  2    .  .    3  .    4  .    5  6    6  .    7  8    8  .    9
;; 4    5  0    0  .    1  2    .  .    3  .    4  .    5  6    6  .    7  8    8  .    9
;;  6666    0000    ....    2222   3333    ....    5555    6666    ....     8888    9999

(def numbers
  {[0 1 2 4 5 6]   0
   [2 5]           1
   [0 2 3 4 6]     2
   [0 2 3 5 6]     3
   [1 2 3 5]       4
   [0 1 3 5 6]     5
   [0 1 3 4 5 6]   6
   [0 2 5]         7
   [0 1 2 3 4 5 6] 8
   [0 1 2 3 5 6]   9})

(defn parse2 [input]
  (->> input
       str/split-lines
       (map #(str/split % #" \| "))
       (map (fn [[s o]] [(str/split s #" ") (str/split o #" ")]))))

(defn decode [segment code]
  (->> segment
       (map #(- (int %) (int \a)))
       (map #(nth code %))
       sort))

(defn solution? [code signal]
  (let [decodes (mapv #(decode % code) signal)]
    (when (every? numbers decodes)
      code)))

(defn solve [[signal output]]
  (let [code (->> (comb/permutations (range 0 7))
                  (keep #(solution? % signal))
                  first)]
    (map #(numbers (decode % code)) output)))

(defn part2
  ([] (part2 sample))
  ([input]
   (->> (parse2 input)
        (map solve)
        (map #(apply str %))
        (map parse-long)
        (reduce +))))

(defn main []
  (println "sample 1: " (part1))
  (println "input 1: " (part1 input))
  (println "sample 2: " (part2))
  (println "input 2: " (part2 input)))

(comment
  (main))
