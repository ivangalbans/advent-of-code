(ns advent-of-code.common.core
  (:require [clojure.java.math :as math]
            [clojure.string :as str]))

(def oo 99999999)

(defn window [coll]
  (map (fn [x y] [x y]) coll (rest coll)))

(defn transpose [board]
  (apply map vector board))

(defn sign [x]
  (if (zero? x)
    0
    (/ x (math/abs x))))

(defn index-at
  ([grid [i j]] (index-at grid i j))
  ([grid i j]   (nth (nth grid i) j)))

(defn inside? [n m [i j]]
  (and (>= i 0) (< i n)
       (>= j 0) (< j m)))

(defn neighbors4-pos [grid i j]
  (let [n (-> grid count)
        m (-> grid first count)]
    (->> [[0 -1] [1 0] [0 1] [-1 0]]
         (map (fn [[x y]] [(+ i x) (+ j y)]))
         (filter (partial inside? n m)))))

(defn neighbors4 [grid i j]
  (->> (neighbors4-pos grid i j)
       (map #(index-at grid %))))

(defn neighbors8-pos [grid [i j]]
  (let [n (-> grid count)
        m (-> grid first count)]
    (->> [[0 -1] [1 0] [0 1] [-1 0] [-1 -1] [1 -1] [1 1] [-1 1]]
         (map (fn [[x y]] [(+ i x) (+ j y)]))
         (filter (partial inside? n m)))))

(defn neighbors8 [grid i j]
  (->> (neighbors8-pos grid [i j])
       (map #(index-at grid %))))

(defn lower-case? [s]
  (= s (str/lower-case s)))
