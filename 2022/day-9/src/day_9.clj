(ns day-9
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def directions
  {"R" [ 1  0]
   "U" [ 0  1]
   "L" [-1  0]
   "D" [ 0 -1]})

(defn- parse-line
  [line]
  (let [[direction number] (str/split line #"\s")]
    [(get directions direction)
     (parse-long number)]))

(defn- normalize-number
  [[direction number]]
  (for [_x (range number)]
    direction))

(defn- move-head
  [knots step]
  (update knots 0 (partial mapv + step)))

(defn- move-knot
  [head knot]
  (let [[dx dy] (map - head knot)]
    (if (and (< (abs dx) 2)
             (< (abs dy) 2))
      knot
      (cond-> knot
        (pos? dx)
        (update 0 inc)

        (neg? dx)
        (update 0 dec)

        (pos? dy)
        (update 1 inc)

        (neg? dy)
        (update 1 dec)))))

(defn- move-knots
  [knots]
  (loop [head      (first knots)
         knots     (rest knots)
         new-knots (into [] [head])]
    (if (seq knots)
      (let [new-knot (move-knot head (first knots))]
        (recur
         new-knot
         (rest knots)
         (conj new-knots new-knot)))
      new-knots)))

(defn- move-rope
  [knots step]
  (-> knots
      (move-head step)
      (move-knots)))

(defn part-one
  []
  (with-open [rdr (io/reader (io/resource "./input.txt"))]
    (let [input (line-seq rdr)
          steps (sequence (comp (map parse-line) (mapcat normalize-number)) input)
          world (into [] (repeat 2 [0 0]))]
      (count
       (into
        #{}
        (map peek)
        (reductions move-rope world steps))))))

(defn part-two
  []
  (with-open [rdr (io/reader (io/resource "./input.txt"))]
    (let [input (line-seq rdr)
          steps (sequence (comp (map parse-line) (mapcat normalize-number)) input)
          world (into [] (repeat 10 [0 0]))]
      (count
       (into
        #{}
        (map peek)
        (reductions move-rope world steps))))))

[(part-one)
 (part-two)]
