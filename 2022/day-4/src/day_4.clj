(ns day-4
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn- fully-contained?
  [[[a b] [x y]]]
  (or
   (and (<= a x)
        (>= b y))
   (and (>= a x)
        (<= b y))))

(defn- helper
  [filter-fn]
  (fn
    [line]
    (let [pairs (str/split line #",")]
      (->> pairs
           (map #(str/split % #"-"))
           (map #(map parse-long %))
           (partition-all 2)
           (filter filter-fn)
           count))))

(defn part-one
  []
  (with-open [rdr (io/reader (io/resource "./input.txt"))]
    (let [input (line-seq rdr)]
      (transduce (map (helper fully-contained?)) + 0 input))))

(defn partially-contained?
  [[[a b] [x y]]]
  (or
   (and
    (>= b x)
    (<= a y))
   (and 
    (<= a y)
    (>= b x))))

(defn part-two
  []
  (with-open [rdr (io/reader (io/resource "./input.txt"))]
    (let [input (line-seq rdr)]
      (transduce (map (helper partially-contained?)) + 0 input))))
