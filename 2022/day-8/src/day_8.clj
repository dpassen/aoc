(ns day-8
  (:require
   [clojure.java.io :as io]))

(defn- numeric-value
  [c]
  (Character/getNumericValue c))

(def split-chars-xf
  (comp
   (map seq)
   (map #(map numeric-value %))
   (map vec)))

(defn- visibilities
  [trees]
  (for [x    (range 1 (dec (count trees)))
        y    (range 1 (dec (count (get trees x))))
        :let [tree (get-in trees [x y])
              row (get trees x)
              column (mapv #(nth % y) trees)
              up (subvec column 0 x)
              down(subvec column (inc x))
              left (subvec row 0 y)
              right (subvec row (inc y))]]
    (for [direction [up left right down]]
      (every? #(> tree %) direction))))

(defn part-one
  []
  (with-open [rdr (io/reader (io/resource "./input.txt"))]
    (let [input     (line-seq rdr)
          trees     (into [] split-chars-xf input)
          perimeter (* 2 (+ (dec (count trees))
                            (dec (count (first trees)))))
          interior  (->> trees visibilities (remove #(every? false? %)) count)]
      (+ perimeter interior))))

(defn- take-until
  [pred coll]
  (transduce (halt-when pred conj) conj coll))

(defn- distances
  [trees]
  (for [x    (range 1 (dec (count trees)))
        y    (range 1 (dec (count (get trees x))))
        :let [tree (get-in trees [x y])
              row (get trees x)
              column (mapv #(nth % y) trees)
              up (rseq (subvec column 0 x))
              down (subvec column (inc x))
              left (rseq (subvec row 0 y))
              right (subvec row (inc y))]]
    (for [direction [up left right down]]
      (count (take-until #(<= tree %) direction)))))

(defn part-two
  []
  (with-open [rdr (io/reader (io/resource "./input.txt"))]
    (let [input     (line-seq rdr)
          trees     (into [] split-chars-xf input)]
      (->> trees
           distances
           (map #(reduce * 1 %))
           (apply max)))))
