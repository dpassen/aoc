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
  [world step]
  (update world :head (partial mapv + step)))

(defn- move-tail
  [{:keys [head tail] :as world}]
  (let [[dx dy] (map - head tail)]
    (if (and (< (abs dx) 2)
             (< (abs dy) 2))
      world
      (cond-> world
        (pos? dx)
        (update-in [:tail 0] inc)

        (neg? dx)
        (update-in [:tail 0] dec)

        (pos? dy)
        (update-in [:tail 1] inc)

        (neg? dy)
        (update-in [:tail 1] dec)))))

(defn- move-rope
  [world step]
  (-> world
      (move-head step)
      (move-tail)))

(defn part-one
  []
  (with-open [rdr (io/reader (io/resource "./input.txt"))]
    (let [input (line-seq rdr)
          steps (sequence (comp (map parse-line) (mapcat normalize-number)) input)
          world {:head [0 0] :tail [0 0]}]
      (count
       (into
        #{}
        (map :tail)
        (reductions move-rope world steps))))))
