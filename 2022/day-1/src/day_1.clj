(ns day-1
  (:require
   [clojure.java.io :as io]))

(def parse-groups-xf
  (comp
   (partition-by empty?)
   (remove #(every? empty? %))
   (map #(transduce (map parse-long) + 0 %))))

(defn part-one
  []
  (with-open [rdr (io/reader (io/resource "./input.txt"))]
    (let [input (line-seq rdr)]
      (transduce parse-groups-xf max 0 input))))

(defn part-two
  []
  (with-open [rdr (io/reader (io/resource "./input.txt"))]
    (let [input    (line-seq rdr)
          unsorted (sequence parse-groups-xf input)
          sorted   (sort #(compare %2 %1) unsorted)]
      (transduce (take 3) + 0 sorted))))

[(part-one)
 (part-two)]
