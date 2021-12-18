(ns advent-of-code.2021.day18
  (:require [advent-of-code.common.core :refer [read-input]]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.zip :as zip]
            [clojure.java.math :as math]
            [clojure.test :refer [deftest are]]))

(def sample
  "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]")

(def input (read-input 2021 18))

(defn parse [input] (->> (str/split-lines input) (mapv edn/read-string)))

(defn half-ceil [v] (-> v (* 0.5) math/ceil int))

(defn half-floor [v] (-> v (* 0.5) math/floor int))

(defn split-regular [n] [(half-floor n) (half-ceil n)])

(defn depth [z] (if (nil? z) -1 (inc (depth (zip/up z)))))

(defn explode? [z]
  (and (= 4 (depth z))
       (vector? (zip/node z))))

(defn left-leaf [z]
  (when z
    (if-let [lz (zip/left z)]
      (loop [node lz]
        (if (number? (zip/node node))
          node
          (recur (zip/rightmost (zip/down node)))))
      (recur (zip/up z)))))

(defn right-leaf [z]
  (when z
    (if-let [rz (zip/right z)]
      (loop [node rz]
        (if (number? (zip/node node))
          node
          (recur (zip/down node))))
      (recur (zip/up z)))))

(defn explode-left [l z]
  (if-let [lz (left-leaf z)]
    (right-leaf (zip/edit lz + l))
    z))

(defn explode-right [r z]
  (if-let [rz (right-leaf z)]
    (zip/edit rz + r)
    z))

(defn explode [number]
  (loop [z (zip/vector-zip number)]
    (when-not (zip/end? z)
      (if (explode? z)
        (let [[l r] (zip/node z)]
          (->> (zip/replace z 0)
               (explode-left l)
               (explode-right r)
               zip/root))
        (recur (zip/next z))))))

(defn split [number]
  (if (vector? number)
    (if-let [l (split (first number))]
      [l (second number)]
      (when-let [r (split (second number))]
        [(first number) r]))
    (when (>= number 10) (split-regular number))))

(defn reduce-snailfish-number [number]
  (or (explode number)
      (split number)
      number))

(defn sum-snailfish-numbers [v1 v2]
  (->> [v1 v2]
       (iterate reduce-snailfish-number)
       (reduce #(if (= %1 %2) (reduced %1) %2))))

(defn magnitude [number]
  (if (number? number)
    number
    (+ (* 3 (magnitude (first  number)))
       (* 2 (magnitude (second number))))))

(defn part1
  ([] (part1 sample))
  ([input]
   (->> (parse input)
        (reduce sum-snailfish-numbers)
        magnitude)))

(defn part2
  ([] (part2 sample))
  ([input]
   (let [numbers (parse input)
         pairs   (for [x numbers y numbers :when (not= x y)] [x y])]
     (->> pairs
          (map (fn [[x y]] (sum-snailfish-numbers x y)))
          (map magnitude)
          (reduce max)))))

(comment
  (part1 input)
  (part2 input))

(deftest part1-sample-test
  (are [result hex] (= result (part1 hex))
    16   "8A004A801A8002F478"
    12   "620080001611562C8802118E34"
    23   "C0015000016115A2E0802F182340"
    31   "A0016C880162017C3686B18A3D4780"))

(deftest explode-test
  (are [expected number] (= expected (explode number))
    [[[[0 9] 2] 3] 4]                 [[[[[9 8] 1] 2] 3] 4]
    [7 [6 [5 [7 0]]]]                 [7 [6 [5 [4 [3 2]]]]]
    [[6 [5 [7 0]]] 3]                 [[6 [5 [4 [3 2]]]] 1]
    [[3 [2 [8 0]]] [9 [5 [4 [3 2]]]]] [[3 [2 [1 [7 3]]]] [6 [5 [4 [3 2]]]]]
    [[[[0 7] 4] [[7 8] [6 0]]] [8 1]] [[[[0 7] 4] [[7 8] [0 [6 7]]]] [1 1]]))

(deftest split-test
  (are [expected number] (= expected (split number))
    [[[[0 7] 4] [[7 8] [0 13]]] [1 1]]    [[[[0 7] 4] [15 [0 13]]] [1 1]]
    [[[[0 7] 4] [[7 8] [0 [6 7]]]] [1 1]] [[[[0 7] 4] [[7 8] [0 13]]] [1 1]]))

(deftest magnitude-test
  (are [expected number] (= expected (magnitude number))
    143    [[1 2] [[3 4] 5]]
    1384   [[[[0 7] 4] [[7 8] [6 0]]] [8 1]]
    445    [[[[1 1] [2 2]] [3 3]] [4 4]]
    791    [[[[3 0] [5 3]] [4 4]] [5 5]]
    1137   [[[[5 0] [7 4]] [5 5]] [6 6]]
    3488   [[[[8 7] [7 7]] [[8 6] [7 7]]] [[[0 7] [6 6]] [8 7]]]
    4140   [[[[6 6] [7 6]] [[7 7] [7 0]]] [[[7 7] [7 7]] [[7 8] [9 9]]]]))

(deftest reduce-snailfish-number-test
  (are [expected number] (reduce-snailfish-number number)
    [[[[0 7] 4] [7 [[8 4] 9]]] [1 1]]     [[[[[4 3] 4] 4] [7 [[8 4] 9]]] [1 1]]
    [[[[0 7] 4] [15 [0 13]]] [1 1]]       [[[[0 7] 4] [7 [[8 4] 9]]] [1 1]]
    [[[[0 7] 4] [[7 8] [0 13]]] [1 1]]    [[[[0 7] 4] [15 [0 13]]] [1 1]]
    [[[[0 7] 4] [[7 8] [0 [6 7]]]] [1 1]] [[[[0 7] 4] [[7 8] [0 13]]] [1 1]]
    [[[[0 7] 4] [[7 8] [6 0]]] [8 1]]     [[[[0 7] 4] [[7 8] [0 [6 7]]]] [1 1]]))
