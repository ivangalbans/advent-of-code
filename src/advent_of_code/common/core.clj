(ns advent-of-code.common.core)

(defn window [coll]
  (map (fn [x y] [x y]) coll (rest coll)))
