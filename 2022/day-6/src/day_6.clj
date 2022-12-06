(ns day-6
  (:require
   [clojure.java.io :as io]))

(defn distinct-subseq
  [line length]
  (->> line
       (partition-all length 1)
       (map-indexed vector)
       (some (fn [[idx chars]]
               (when (= length (count (distinct chars)))
                 (+ idx length))))))

(defn part-1
  []
  (with-open [rdr (io/reader (io/resource "./input.txt"))]
    (let [input (slurp rdr)]
      (distinct-subseq input 4))))

(defn part-2
  []
  (with-open [rdr (io/reader (io/resource "./input.txt"))]
    (let [input (slurp rdr)]
      (distinct-subseq input 14))))
