(ns day-3
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]))

(defn- char-range
  [start end]
  (map char (range (int start) (inc (int end)))))

(def priorities
  (merge
   (zipmap
    (char-range \a \z)
    (range 1 27))
   (zipmap
    (char-range \A \Z)
    (range 27 53))))

(defn- common-priorities
  [xs]
  (->> xs
       (map set)
       (apply set/intersection)
       (map priorities)))

(defn- parse-rucksacks
  [line]
  (let [midpoint (/ (count line) 2)]
    (split-at midpoint line)))

(defn part-one
  []
  (with-open [rdr (io/reader (io/resource "./input.txt"))]
    (let [input (line-seq rdr)]
      (transduce (comp (map parse-rucksacks) (mapcat common-priorities)) + 0 input))))

(defn part-two
  []
  (with-open [rdr (io/reader (io/resource "./input.txt"))]
    (let [input (line-seq rdr)]
      (transduce (comp (partition-all 3) (mapcat common-priorities)) + 0 input))))
