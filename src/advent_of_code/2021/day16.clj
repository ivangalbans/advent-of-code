(ns advent-of-code.2021.day16
  (:require [clojure.test :refer [deftest are]]
            [advent-of-code.common.core :refer [read-input]]
            [clojure.string :as str]))

(def sample "D2FE28")

(def input (read-input 2021 16))

(defn parse [input]
  (str/trim-newline input))

(def hexchar->bin
  {\0 "0000"
   \1 "0001"
   \2 "0010"
   \3 "0011"
   \4 "0100"
   \5 "0101"
   \6 "0110"
   \7 "0111"
   \8 "1000"
   \9 "1001"
   \A "1010"
   \B "1011"
   \C "1100"
   \D "1101"
   \E "1110"
   \F "1111"})

(defn hex->bin [hex]
  (->> hex (map hexchar->bin) (apply str)))

(defn bin->dec [bin]
  (Integer/parseInt bin 2))

(defn take-bits [n bits]
  [(subs bits 0 n) (subs bits n)])

(defn parse-bits [n bits]
  (let [[preffix suffix] (take-bits n bits)]
    [(bin->dec preffix) suffix]))

(declare parse-packet)

(defn parse-count-packets [n bits]
  (loop [packets []
         bits bits]
    (if (= n (count packets))
      [packets bits]
      (let [[packet bits] (parse-packet bits)]
        (recur (conj packets packet) bits)))))

(defn parse-len-packets [n bits]
  (let [[bits rest] (take-bits n bits)]
    (loop [packets []
           bits bits]
      (if (zero? (count bits))
        [packets rest]
        (let [[packet bits] (parse-packet bits)]
          (recur (conj packets packet) bits))))))

(defn parse-operation [bits]
  (let [[length-type-id bits] (parse-bits 1 bits)
        [packets-parser len] (case length-type-id
                               0 [parse-len-packets 15]
                               1 [parse-count-packets 11])
        [n bits] (parse-bits len bits)]
    (packets-parser n bits)))

(defn parse-literal [bits]
  (loop [value 0
         bits  bits]
    (let [[bit1 bits] (parse-bits 1 bits)
          [bit4 bits] (parse-bits 4 bits)
          value       (+ bit4 (bit-shift-left value 4))]
      (case bit1
        0 [value bits]
        1 (recur value bits)))))

(defn parse-packet [bits]
  (let [[version bits] (parse-bits 3 bits)
        [type-id bits] (parse-bits 3 bits)
        [packet packets] (if (= 4 type-id)
                           (parse-literal bits)
                           (parse-operation bits))]
    [{:version version
      :type-id type-id
      :packet packet}
     packets]))

(defn sum-versions [{:keys [version type-id packet]}]
  (case type-id
    4 version
    (->> packet (map sum-versions) (reduce + version))))

(def bool->int {true 1 false 0})

(defn evaluate [{:keys [type-id packet]}]
  (case type-id
    4 packet
    0 (->> packet (map evaluate) (reduce +))
    1 (->> packet (map evaluate) (reduce *))
    2 (->> packet (map evaluate) (reduce min))
    3 (->> packet (map evaluate) (reduce max))
    5 (->> packet (map evaluate) (reduce >) bool->int)
    6 (->> packet (map evaluate) (reduce <) bool->int)
    7 (->> packet (map evaluate) (reduce =) bool->int)))

(defn solve [op input]
  (-> input parse hex->bin parse-packet first op))

(defn part1
  ([]      (part1 sample))
  ([input]
   (solve sum-versions input)))

(defn part2
  ([]      (part2 sample))
  ([input] (solve evaluate input)))

(defn main []
  (println "sample 1:" (part1))
  (println "input 1:" (part1 input))
  (println "sample 2:" (part2))
  (println "input 2:" (part2 input)))

(comment
  (main))

(deftest part1-sample-test
  (are [result hex] (= result (part1 hex))
    16   "8A004A801A8002F478"
    12   "620080001611562C8802118E34"
    23   "C0015000016115A2E0802F182340"
    31   "A0016C880162017C3686B18A3D4780"))

(deftest part2-sample-test
  (are [result hex] (= result (part2 hex))
    3   "C200B40A82"
    54  "04005AC33890"
    7   "880086C3E88112"
    9   "CE00C43D881120"
    1   "D8005AC2A8F0"
    0   "F600BC2D8F"
    0   "9C005AC2F8F0"
    1   "9C0141080250320F1802104A08"))
